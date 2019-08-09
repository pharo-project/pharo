"
I am ZnMimeType.
Mime types are an element used in describing resources, more specifically their format.

Here are some examples MIME types:

	text/plain
	text/html
	text/plain;charset=utf-8
	image/png
	text/*
 
A mime type has a main/sub form with optional parameters. 

For more information: http://en.wikipedia.org/wiki/MIME_type
I know whether I am binary or not and know my charset when applicable.
My class side offers access to some common constants.
Note that for text types we default to UTF-8 encoding.

Examples:

	ZnMimeType textPlain = 'text/plain;charset=utf-8' asZnMimeType.
	ZnMimeType textPlain charSet.
	ZnMimeType textHtml matches: ZnMimeType text.
	ZnMimeType default matches: ZnMimeType any.
	ZnMimeType imagePng isBinary.
	ZnMimeType forFilenameExtension: 'html'.

I started life as a copy of WAMineType.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnMimeType,
	#superclass : #Object,
	#instVars : [
		'main',
		'sub',
		'parameters'
	],
	#classVars : [
		'ExtensionsMap',
		'TextSubTypeFragments'
	],
	#category : #'Zinc-Resource-Meta-Core'
}

{ #category : #convenience }
ZnMimeType class >> any [
	"This wildcard instance #matches: any mime type"
	
	^ self main: '*' sub: '*'
]

{ #category : #convenience }
ZnMimeType class >> applicationFormUrlEncoded [
	^ self main: 'application' sub: 'x-www-form-urlencoded'
]

{ #category : #convenience }
ZnMimeType class >> applicationJavascript [
	^ self main: 'application' sub: 'javascript'
]

{ #category : #convenience }
ZnMimeType class >> applicationJson [
	^ self main: 'application' sub: 'json'
]

{ #category : #convenience }
ZnMimeType class >> applicationOctetStream [
	^ self main: 'application' sub: 'octet-stream'
]

{ #category : #convenience }
ZnMimeType class >> applicationPdf [
	^ self main: 'application' sub: 'pdf'
]

{ #category : #convenience }
ZnMimeType class >> applicationSton [
	^ self main: 'application' sub: 'ston'
]

{ #category : #convenience }
ZnMimeType class >> applicationXml [
	^ self main: 'application' sub: 'xml'
]

{ #category : #accessing }
ZnMimeType class >> commonMimeTypes [
	"Return a list of commom mime types, the ones we provided convenience accessors for."

	^ #( 
			applicationFormUrlEncoded applicationJavascript applicationJson 
			applicationOctetStream applicationPdf applicationXml 
			imageGif imageJpeg imagePng imageSvg 
			textCss textHtml textJavascript textPlain 
		) collect: [ :each | self perform: each ]
]

{ #category : #convenience }
ZnMimeType class >> default [
	^ self applicationOctetStream 
]

{ #category : #private }
ZnMimeType class >> extensionsMap [
	ExtensionsMap ifNil: [ self initializeExtensionsMap ].
	^ ExtensionsMap
]

{ #category : #'instance creation' }
ZnMimeType class >> forFilenameExtension: extension [
	^ self 
		forFilenameExtension: extension 
		ifAbsent: [ self default ]
]

{ #category : #'instance creation' }
ZnMimeType class >> forFilenameExtension: extension ifAbsent: block [
	^ self extensionsMap 
		at: extension 
		ifAbsent: block
]

{ #category : #'instance creation' }
ZnMimeType class >> fromString: aString [ 
	| main endOfSub sub parts parameters |
	main := aString copyUpTo: $/.
	endOfSub := aString indexOf: $;.
	endOfSub := endOfSub = 0 
		ifTrue: [ aString size ]
		ifFalse: [ endOfSub - 1 ].
	sub := aString copyFrom: main size + 2 to: endOfSub.
	endOfSub = aString size ifTrue: [ ^ self main: main sub: sub ].
	parts := (aString copyFrom: endOfSub + 1 to: aString size) substrings: ';'.
	parameters := SmallDictionary new.
	parts do: [ :each | 
		parameters
			at: (each copyUpTo: $=) trimBoth 
			put: (each copyAfter: $=) trimBoth ].
	^ self 
		main: main
		sub: sub
		parameters: parameters
]

{ #category : #convenience }
ZnMimeType class >> imageGif [
	^ self main: 'image' sub: 'gif'
]

{ #category : #convenience }
ZnMimeType class >> imageJpeg [
	^ self main: 'image' sub: 'jpeg'
]

{ #category : #convenience }
ZnMimeType class >> imagePng [
	^ self main: 'image' sub: 'png'
]

{ #category : #convenience }
ZnMimeType class >> imageSvg [
	^ self main: 'image' sub: 'svg+xml'
]

{ #category : #'class initialization' }
ZnMimeType class >> initialize [
	"Last forced update: 20170829"
	
	self initializeTextSubTypeFragments
]

{ #category : #private }
ZnMimeType class >> initializeExtensionsMap [
	"Create a mapping between filename extensions and mime types"

	ExtensionsMap := Dictionary new.
	self mimeTypeFilenameExtensionsSpec
		do: [ :each | 
			| mimeType |
			mimeType := self fromString: each first.
			each allButFirstDo: [ :extension | ExtensionsMap at: extension put: mimeType ] ].
	self patchExtensionsMap
]

{ #category : #private }
ZnMimeType class >> initializeTextSubTypeFragments [
	TextSubTypeFragments := #('javascript' 'json' 'ston' 'html' 'xml' 'sgml' 'yaml' 'csv') asSet
]

{ #category : #accessing }
ZnMimeType class >> knownFilenameExtensions [
	"Return a collection of the filename extensions that we know of"
	
	^ self extensionsMap keys
]

{ #category : #accessing }
ZnMimeType class >> knownMimeTypesWithFilenameExtensions [
	"Return a collection of mime types for which we know a filename extension"
	
	^ self extensionsMap values
]

{ #category : #'instance creation' }
ZnMimeType class >> main: aMainString sub: aSubstring [
	^ self new
		main: aMainString;
		sub: aSubstring;
		yourself
]

{ #category : #'instance creation' }
ZnMimeType class >> main: aMainString sub: aSubstring parameters: aDictionary [
	^ (self main: aMainString sub: aSubstring)
		parameters: aDictionary;
		yourself
]

{ #category : #private }
ZnMimeType class >> mimeTypeFilenameExtensionsSpec [
	"This array maps mime types to a list of filename extensions, based on the Debian/Ubuntu /etc/mime.types file"
	
"('mime.types' asFileReference readStreamDo: [ :in | in contentsOfEntireFile ]) lines 
	collect: [ :line |
		(line isEmpty not and: [ line first ~= $# ])
			ifTrue: [
				(line findTokens: { Character space. Character tab }) asArray ] ]
	thenSelect: [ :each | each notNil and: [ each size > 1 ] ]."
	
	^ #(#('application/andrew-inset' 'ez') #('application/annodex' 'anx') #('application/atom+xml' 'atom') #('application/atomcat+xml' 'atomcat') #('application/atomserv+xml' 'atomsrv') #('application/bbolin' 'lin') #('application/cu-seeme' 'cu') #('application/davmount+xml' 'davmount') #('application/dicom' 'dcm') #('application/dsptype' 'tsp') #('application/ecmascript' 'es') #('application/futuresplash' 'spl') #('application/hta' 'hta') #('application/java-archive' 'jar') #('application/java-serialized-object' 'ser') #('application/java-vm' 'class') #('application/javascript' 'js') #('application/json' 'json') #('application/m3g' 'm3g') #('application/mac-binhex40' 'hqx') #('application/mac-compactpro' 'cpt') #('application/mathematica' 'nb' 'nbp') #('application/mbox' 'mbox') #('application/msaccess' 'mdb') #('application/msword' 'doc' 'dot') #('application/mxf' 'mxf') #('application/octet-stream' 'bin') #('application/oda' 'oda') #('application/ogg' 'ogx') #('application/onenote' 'one' 'onetoc2' 'onetmp' 'onepkg') #('application/pdf' 'pdf') #('application/pgp-encrypted' 'pgp') #('application/pgp-keys' 'key') #('application/pgp-signature' 'sig') #('application/pics-rules' 'prf') #('application/postscript' 'ps' 'ai' 'eps' 'epsi' 'epsf' 'eps2' 'eps3') #('application/rar' 'rar') #('application/rdf+xml' 'rdf') #('application/rtf' 'rtf') #('application/sla' 'stl') #('application/smil' 'smi' 'smil') #('application/xhtml+xml' 'xhtml' 'xht') #('application/xml' 'xml' 'xsl' 'xsd') #('application/xspf+xml' 'xspf') #('application/zip' 'zip') #('application/vnd.android.package-archive' 'apk') #('application/vnd.cinderella' 'cdy') #('application/vnd.google-earth.kml+xml' 'kml') #('application/vnd.google-earth.kmz' 'kmz') #('application/vnd.mozilla.xul+xml' 'xul') #('application/vnd.ms-excel' 'xls' 'xlb' 'xlt') #('application/vnd.ms-excel.addin.macroEnabled.12' 'xlam') #('application/vnd.ms-excel.sheet.binary.macroEnabled.12' 'xlsb') #('application/vnd.ms-excel.sheet.macroEnabled.12' 'xlsm') #('application/vnd.ms-excel.template.macroEnabled.12' 'xltm') #('application/vnd.ms-fontobject' 'eot') #('application/vnd.ms-officetheme' 'thmx') #('application/vnd.ms-pki.seccat' 'cat') #('application/vnd.ms-powerpoint' 'ppt' 'pps') #('application/vnd.ms-powerpoint.addin.macroEnabled.12' 'ppam') #('application/vnd.ms-powerpoint.presentation.macroEnabled.12' 'pptm') #('application/vnd.ms-powerpoint.slide.macroEnabled.12' 'sldm') #('application/vnd.ms-powerpoint.slideshow.macroEnabled.12' 'ppsm') #('application/vnd.ms-powerpoint.template.macroEnabled.12' 'potm') #('application/vnd.ms-word.document.macroEnabled.12' 'docm') #('application/vnd.ms-word.template.macroEnabled.12' 'dotm') #('application/vnd.oasis.opendocument.chart' 'odc') #('application/vnd.oasis.opendocument.database' 'odb') #('application/vnd.oasis.opendocument.formula' 'odf') #('application/vnd.oasis.opendocument.graphics' 'odg') #('application/vnd.oasis.opendocument.graphics-template' 'otg') #('application/vnd.oasis.opendocument.image' 'odi') #('application/vnd.oasis.opendocument.presentation' 'odp') #('application/vnd.oasis.opendocument.presentation-template' 'otp') #('application/vnd.oasis.opendocument.spreadsheet' 'ods') #('application/vnd.oasis.opendocument.spreadsheet-template' 'ots') #('application/vnd.oasis.opendocument.text' 'odt') #('application/vnd.oasis.opendocument.text-master' 'odm') #('application/vnd.oasis.opendocument.text-template' 'ott') #('application/vnd.oasis.opendocument.text-web' 'oth') #('application/vnd.openxmlformats-officedocument.presentationml.presentation' 'pptx') #('application/vnd.openxmlformats-officedocument.presentationml.slide' 'sldx') #('application/vnd.openxmlformats-officedocument.presentationml.slideshow' 'ppsx') #('application/vnd.openxmlformats-officedocument.presentationml.template' 'potx') #('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' 'xlsx') #('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' 'xlsx') #('application/vnd.openxmlformats-officedocument.spreadsheetml.template' 'xltx') #('application/vnd.openxmlformats-officedocument.spreadsheetml.template' 'xltx') #('application/vnd.openxmlformats-officedocument.wordprocessingml.document' 'docx') #('application/vnd.openxmlformats-officedocument.wordprocessingml.template' 'dotx') #('application/vnd.rim.cod' 'cod') #('application/vnd.smaf' 'mmf') #('application/vnd.stardivision.calc' 'sdc') #('application/vnd.stardivision.chart' 'sds') #('application/vnd.stardivision.draw' 'sda') #('application/vnd.stardivision.impress' 'sdd') #('application/vnd.stardivision.math' 'sdf') #('application/vnd.stardivision.writer' 'sdw') #('application/vnd.stardivision.writer-global' 'sgl') #('application/vnd.sun.xml.calc' 'sxc') #('application/vnd.sun.xml.calc.template' 'stc') #('application/vnd.sun.xml.draw' 'sxd') #('application/vnd.sun.xml.draw.template' 'std') #('application/vnd.sun.xml.impress' 'sxi') #('application/vnd.sun.xml.impress.template' 'sti') #('application/vnd.sun.xml.math' 'sxm') #('application/vnd.sun.xml.writer' 'sxw') #('application/vnd.sun.xml.writer.global' 'sxg') #('application/vnd.sun.xml.writer.template' 'stw') #('application/vnd.symbian.install' 'sis') #('application/vnd.tcpdump.pcap' 'cap' 'pcap') #('application/vnd.visio' 'vsd') #('application/vnd.wap.wbxml' 'wbxml') #('application/vnd.wap.wmlc' 'wmlc') #('application/vnd.wap.wmlscriptc' 'wmlsc') #('application/vnd.wordperfect' 'wpd') #('application/vnd.wordperfect5.1' 'wp5') #('application/x-123' 'wk') #('application/x-7z-compressed' '7z') #('application/x-abiword' 'abw') #('application/x-apple-diskimage' 'dmg') #('application/x-bcpio' 'bcpio') #('application/x-bittorrent' 'torrent') #('application/x-cab' 'cab') #('application/x-cbr' 'cbr') #('application/x-cbz' 'cbz') #('application/x-cdf' 'cdf' 'cda') #('application/x-cdlink' 'vcd') #('application/x-chess-pgn' 'pgn') #('application/x-comsol' 'mph') #('application/x-cpio' 'cpio') #('application/x-csh' 'csh') #('application/x-debian-package' 'deb' 'udeb') #('application/x-director' 'dcr' 'dir' 'dxr') #('application/x-dms' 'dms') #('application/x-doom' 'wad') #('application/x-dvi' 'dvi') #('application/x-font' 'pfa' 'pfb' 'gsf' 'pcf' 'pcf.Z') #('application/x-font-woff' 'woff') #('application/x-freemind' 'mm') #('application/x-futuresplash' 'spl') #('application/x-ganttproject' 'gan') #('application/x-gnumeric' 'gnumeric') #('application/x-go-sgf' 'sgf') #('application/x-graphing-calculator' 'gcf') #('application/x-gtar' 'gtar') #('application/x-gtar-compressed' 'tgz' 'taz') #('application/x-hdf' 'hdf') #('application/x-hwp' 'hwp') #('application/x-ica' 'ica') #('application/x-info' 'info') #('application/x-internet-signup' 'ins' 'isp') #('application/x-iphone' 'iii') #('application/x-iso9660-image' 'iso') #('application/x-jam' 'jam') #('application/x-java-jnlp-file' 'jnlp') #('application/x-jmol' 'jmz') #('application/x-kchart' 'chrt') #('application/x-killustrator' 'kil') #('application/x-koan' 'skp' 'skd' 'skt' 'skm') #('application/x-kpresenter' 'kpr' 'kpt') #('application/x-kspread' 'ksp') #('application/x-kword' 'kwd' 'kwt') #('application/x-latex' 'latex') #('application/x-lha' 'lha') #('application/x-lyx' 'lyx') #('application/x-lzh' 'lzh') #('application/x-lzx' 'lzx') #('application/x-maker' 'frm' 'maker' 'frame' 'fm' 'fb' 'book' 'fbdoc') #('application/x-md5' 'md5') #('application/x-mif' 'mif') #('application/x-mpegURL' 'm3u8') #('application/x-ms-wmd' 'wmd') #('application/x-ms-wmz' 'wmz') #('application/x-msdos-program' 'com' 'exe' 'bat' 'dll') #('application/x-msi' 'msi') #('application/x-netcdf' 'nc') #('application/x-ns-proxy-autoconfig' 'pac' 'dat') #('application/x-nwc' 'nwc') #('application/x-object' 'o') #('application/x-oz-application' 'oza') #('application/x-pkcs7-certreqresp' 'p7r') #('application/x-pkcs7-crl' 'crl') #('application/x-python-code' 'pyc' 'pyo') #('application/x-qgis' 'qgs' 'shp' 'shx') #('application/x-quicktimeplayer' 'qtl') #('application/x-rdp' 'rdp') #('application/x-redhat-package-manager' 'rpm') #('application/x-rss+xml' 'rss') #('application/x-ruby' 'rb') #('application/x-scilab' 'sci' 'sce') #('application/x-scilab-xcos' 'xcos') #('application/x-sh' 'sh') #('application/x-sha1' 'sha1') #('application/x-shar' 'shar') #('application/x-shockwave-flash' 'swf' 'swfl') #('application/x-silverlight' 'scr') #('application/x-sql' 'sql') #('application/x-stuffit' 'sit' 'sitx') #('application/x-sv4cpio' 'sv4cpio') #('application/x-sv4crc' 'sv4crc') #('application/x-tar' 'tar') #('application/x-tcl' 'tcl') #('application/x-tex-gf' 'gf') #('application/x-tex-pk' 'pk') #('application/x-texinfo' 'texinfo' 'texi') #('application/x-trash' '~' '%' 'bak' 'old' 'sik') #('application/x-troff' 't' 'tr' 'roff') #('application/x-troff-man' 'man') #('application/x-troff-me' 'me') #('application/x-troff-ms' 'ms') #('application/x-ustar' 'ustar') #('application/x-wais-source' 'src') #('application/x-wingz' 'wz') #('application/x-x509-ca-cert' 'crt') #('application/x-xcf' 'xcf') #('application/x-xfig' 'fig') #('application/x-xpinstall' 'xpi') #('audio/amr' 'amr') #('audio/amr-wb' 'awb') #('audio/amr' 'amr') #('audio/amr-wb' 'awb') #('audio/annodex' 'axa') #('audio/basic' 'au' 'snd') #('audio/csound' 'csd' 'orc' 'sco') #('audio/flac' 'flac') #('audio/midi' 'mid' 'midi' 'kar') #('audio/mpeg' 'mpga' 'mpega' 'mp2' 'mp3' 'm4a') #('audio/mpegurl' 'm3u') #('audio/ogg' 'oga' 'ogg' 'spx') #('audio/prs.sid' 'sid') #('audio/x-aiff' 'aif' 'aiff' 'aifc') #('audio/x-gsm' 'gsm') #('audio/x-mpegurl' 'm3u') #('audio/x-ms-wma' 'wma') #('audio/x-ms-wax' 'wax') #('audio/x-pn-realaudio' 'ra' 'rm' 'ram') #('audio/x-realaudio' 'ra') #('audio/x-scpls' 'pls') #('audio/x-sd2' 'sd2') #('audio/x-wav' 'wav') #('chemical/x-alchemy' 'alc') #('chemical/x-cache' 'cac' 'cache') #('chemical/x-cache-csf' 'csf') #('chemical/x-cactvs-binary' 'cbin' 'cascii' 'ctab') #('chemical/x-cdx' 'cdx') #('chemical/x-cerius' 'cer') #('chemical/x-chem3d' 'c3d') #('chemical/x-chemdraw' 'chm') #('chemical/x-cif' 'cif') #('chemical/x-cmdf' 'cmdf') #('chemical/x-cml' 'cml') #('chemical/x-compass' 'cpa') #('chemical/x-crossfire' 'bsd') #('chemical/x-csml' 'csml' 'csm') #('chemical/x-ctx' 'ctx') #('chemical/x-cxf' 'cxf' 'cef') #('chemical/x-embl-dl-nucleotide' 'emb' 'embl') #('chemical/x-galactic-spc' 'spc') #('chemical/x-gamess-input' 'inp' 'gam' 'gamin') #('chemical/x-gaussian-checkpoint' 'fch' 'fchk') #('chemical/x-gaussian-cube' 'cub') #('chemical/x-gaussian-input' 'gau' 'gjc' 'gjf') #('chemical/x-gaussian-log' 'gal') #('chemical/x-gcg8-sequence' 'gcg') #('chemical/x-genbank' 'gen') #('chemical/x-hin' 'hin') #('chemical/x-isostar' 'istr' 'ist') #('chemical/x-jcamp-dx' 'jdx' 'dx') #('chemical/x-kinemage' 'kin') #('chemical/x-macmolecule' 'mcm') #('chemical/x-macromodel-input' 'mmd' 'mmod') #('chemical/x-mdl-molfile' 'mol') #('chemical/x-mdl-rdfile' 'rd') #('chemical/x-mdl-rxnfile' 'rxn') #('chemical/x-mdl-sdfile' 'sd' 'sdf') #('chemical/x-mdl-tgf' 'tgf') #('chemical/x-mmcif' 'mcif') #('chemical/x-mol2' 'mol2') #('chemical/x-molconn-Z' 'b') #('chemical/x-mopac-graph' 'gpt') #('chemical/x-mopac-input' 'mop' 'mopcrt' 'mpc' 'zmt') #('chemical/x-mopac-out' 'moo') #('chemical/x-mopac-vib' 'mvb') #('chemical/x-ncbi-asn1' 'asn') #('chemical/x-ncbi-asn1-ascii' 'prt' 'ent') #('chemical/x-ncbi-asn1-binary' 'val' 'aso') #('chemical/x-ncbi-asn1-spec' 'asn') #('chemical/x-pdb' 'pdb' 'ent') #('chemical/x-rosdal' 'ros') #('chemical/x-swissprot' 'sw') #('chemical/x-vamas-iso14976' 'vms') #('chemical/x-vmd' 'vmd') #('chemical/x-xtel' 'xtel') #('chemical/x-xyz' 'xyz') #('image/gif' 'gif') #('image/ief' 'ief') #('image/jpeg' 'jpeg' 'jpg' 'jpe') #('image/pcx' 'pcx') #('image/png' 'png') #('image/svg+xml' 'svg' 'svgz') #('image/tiff' 'tiff' 'tif') #('image/vnd.djvu' 'djvu' 'djv') #('image/vnd.microsoft.icon' 'ico') #('image/vnd.wap.wbmp' 'wbmp') #('image/x-canon-cr2' 'cr2') #('image/x-canon-crw' 'crw') #('image/x-cmu-raster' 'ras') #('image/x-coreldraw' 'cdr') #('image/x-coreldrawpattern' 'pat') #('image/x-coreldrawtemplate' 'cdt') #('image/x-corelphotopaint' 'cpt') #('image/x-epson-erf' 'erf') #('image/x-jg' 'art') #('image/x-jng' 'jng') #('image/x-ms-bmp' 'bmp') #('image/x-nikon-nef' 'nef') #('image/x-olympus-orf' 'orf') #('image/x-photoshop' 'psd') #('image/x-portable-anymap' 'pnm') #('image/x-portable-bitmap' 'pbm') #('image/x-portable-graymap' 'pgm') #('image/x-portable-pixmap' 'ppm') #('image/x-rgb' 'rgb') #('image/x-xbitmap' 'xbm') #('image/x-xpixmap' 'xpm') #('image/x-xwindowdump' 'xwd') #('message/rfc822' 'eml') #('model/iges' 'igs' 'iges') #('model/mesh' 'msh' 'mesh' 'silo') #('model/vrml' 'wrl' 'vrml') #('model/x3d+vrml' 'x3dv') #('model/x3d+xml' 'x3d') #('model/x3d+binary' 'x3db') #('text/cache-manifest' 'appcache') #('text/calendar' 'ics' 'icz') #('text/css' 'css') #('text/csv' 'csv') #('text/h323' '323') #('text/html' 'html' 'htm' 'shtml') #('text/iuls' 'uls') #('text/mathml' 'mml') #('text/plain' 'asc' 'txt' 'text' 'pot' 'brf' 'srt') #('text/richtext' 'rtx') #('text/scriptlet' 'sct' 'wsc') #('text/texmacs' 'tm') #('text/tab-separated-values' 'tsv') #('text/vnd.sun.j2me.app-descriptor' 'jad') #('text/vnd.wap.wml' 'wml') #('text/vnd.wap.wmlscript' 'wmls') #('text/x-bibtex' 'bib') #('text/x-boo' 'boo') #('text/x-c++hdr' 'h++' 'hpp' 'hxx' 'hh') #('text/x-c++src' 'c++' 'cpp' 'cxx' 'cc') #('text/x-chdr' 'h') #('text/x-component' 'htc') #('text/x-csh' 'csh') #('text/x-csrc' 'c') #('text/x-dsrc' 'd') #('text/x-diff' 'diff' 'patch') #('text/x-haskell' 'hs') #('text/x-java' 'java') #('text/x-lilypond' 'ly') #('text/x-literate-haskell' 'lhs') #('text/x-moc' 'moc') #('text/x-pascal' 'p' 'pas') #('text/x-pcs-gcd' 'gcd') #('text/x-perl' 'pl' 'pm') #('text/x-python' 'py') #('text/x-scala' 'scala') #('text/x-setext' 'etx') #('text/x-sfv' 'sfv') #('text/x-sh' 'sh') #('text/x-tcl' 'tcl' 'tk') #('text/x-tex' 'tex' 'ltx' 'sty' 'cls') #('text/x-vcalendar' 'vcs') #('text/x-vcard' 'vcf') #('video/3gpp' '3gp') #('video/annodex' 'axv') #('video/dl' 'dl') #('video/dv' 'dif' 'dv') #('video/fli' 'fli') #('video/gl' 'gl') #('video/mpeg' 'mpeg' 'mpg' 'mpe') #('video/MP2T' 'ts') #('video/mp4' 'mp4') #('video/quicktime' 'qt' 'mov') #('video/ogg' 'ogv') #('video/webm' 'webm') #('video/vnd.mpegurl' 'mxu') #('video/x-flv' 'flv') #('video/x-la-asf' 'lsf' 'lsx') #('video/x-mng' 'mng') #('video/x-ms-asf' 'asf' 'asx') #('video/x-ms-wm' 'wm') #('video/x-ms-wmv' 'wmv') #('video/x-ms-wmx' 'wmx') #('video/x-ms-wvx' 'wvx') #('video/x-msvideo' 'avi') #('video/x-sgi-movie' 'movie') #('video/x-matroska' 'mpv' 'mkv') #('x-conference/x-cooltalk' 'ice') #('x-epoc/x-sisx-app' 'sisx') #('x-world/x-vrml' 'vrm' 'vrml' 'wrl'))
]

{ #category : #convenience }
ZnMimeType class >> multiPartFormData [
	^ self main: 'multipart' sub: 'form-data'
]

{ #category : #private }
ZnMimeType class >> patchExtensionsMap [
	"Patch the extension map that we read from the spec"
	
	#('~' '%') do: [ :each | ExtensionsMap removeKey: each ].
	(ExtensionsMap at: 'text') setCharSetUTF8.
	(ExtensionsMap at: 'html') setCharSetUTF8.
	ExtensionsMap at: 'asc' put: 'text/plain' asZnMimeType
]

{ #category : #convenience }
ZnMimeType class >> text [
	"This wildcard instance #matches: all textual mime types"
	
	^ self main: 'text' sub: '*'
]

{ #category : #convenience }
ZnMimeType class >> textCss [
	^ self main: 'text' sub: 'css'
]

{ #category : #convenience }
ZnMimeType class >> textHtml [
	"Note that the charset is set to utf8 automatically"
	
	^ (self main: 'text' sub: 'html') 
		setCharSetUTF8; 
		yourself
]

{ #category : #convenience }
ZnMimeType class >> textJavascript [
	^ self main: 'text' sub: 'javascript'
]

{ #category : #convenience }
ZnMimeType class >> textPlain [
	"Note that the charset is set to utf8 automatically"

	^ (self main: 'text' sub: 'plain') 
		setCharSetUTF8; 
		yourself
]

{ #category : #private }
ZnMimeType class >> textSubTypeFragments [
	"Return a set of fragments of sub types that indicate for any main type
	that the mime type is non-binary (thus textual)"
	
	^ TextSubTypeFragments
]

{ #category : #comparing }
ZnMimeType >> = other [
	"#matches should be used instead of #= to ignore parameters."

	^ (self class == other class)
		and: [ self main = other main
			and: [ self sub = other sub 
				and: [ self hasSameParametersAs: other ] ] ]
]

{ #category : #converting }
ZnMimeType >> asMIMEType [
	^ self
]

{ #category : #converting }
ZnMimeType >> asZnMimeType [
	^ self
]

{ #category : #parameters }
ZnMimeType >> charSet [
	^ self parameterAt: 'charset' ifAbsent: [ nil ]
]

{ #category : #parameters }
ZnMimeType >> charSet: aString [
	self parameters at: 'charset' put: aString
]

{ #category : #parameters }
ZnMimeType >> clearCharSet [
	self removeParameter: 'charset'
]

{ #category : #testing }
ZnMimeType >> hasParameters [
	^ parameters notNil and: [ parameters isEmpty not ]
]

{ #category : #testing }
ZnMimeType >> hasSameParametersAs: aMimeType [
	^ self hasParameters = aMimeType hasParameters
		and: [self parameters size = aMimeType parameters size
			and: [self parameters associations allSatisfy: [:each |
				aMimeType parameters includesAssociation: each]]]
]

{ #category : #comparing }
ZnMimeType >> hash [
	| hash |
	hash := self main hash bitXor: self sub hash.
	self hasParameters 
		ifTrue: [ hash := hash bitXor: self parameters hash ].
	^ hash
]

{ #category : #testing }
ZnMimeType >> isBinary [
	"Answers whether the contents of a resource described by me 
	should be considered as binary instead of textual."

	self main = 'text'
		ifTrue: [ ^ false ].
	^ (self sub substrings: '+-')
		noneSatisfy: [ :each | self class textSubTypeFragments includes: each ]
]

{ #category : #testing }
ZnMimeType >> isCharSetUTF8 [
	^ self charSet notNil and: [ self charSet sameAs: 'utf-8' ]
]

{ #category : #testing }
ZnMimeType >> isNonStandard [
	"Tests if the receiver is a non-standard mime type,
	i.e. one that is not registered with IANA"
	
	^ (self main beginsWith: 'x-')
		or: [ (self main beginsWith: 'X-')
		or: [ (self sub beginsWith: 'x-')
		or: [ self sub beginsWith: 'X-' ] ] ]
]

{ #category : #testing }
ZnMimeType >> isVendorSpecific [
	"Tests if the receiver is a vendor specific mime type"
	
	^ self sub beginsWith: 'vnd.'
]

{ #category : #accessing }
ZnMimeType >> main [
	^ main
]

{ #category : #accessing }
ZnMimeType >> main: aString [
	main := aString asLowercase
]

{ #category : #testing }
ZnMimeType >> matches: aMimeType [
	"Compare the receiver to aMimeType, which is the pattern to match, 
	it is a normal instance where main or sub can be wildcards.
	Matches should be used instead of #= to ignore parameters."
	
	^ (aMimeType main = '*'
		or: [ aMimeType main = self main ])
		and: [
			aMimeType sub = '*'
				or: [ aMimeType sub = self sub ] ]
]

{ #category : #parameters }
ZnMimeType >> parameterAt: key [
	^ self parameterAt: key ifAbsent: [ KeyNotFound signalFor: key ]
]

{ #category : #parameters }
ZnMimeType >> parameterAt: key ifAbsent: block [
	parameters isNil ifTrue: [ ^ block value ].
	^ self parameters at: key ifAbsent: block
]

{ #category : #parameters }
ZnMimeType >> parameterAt: aKey put: aValue [
	self parameters at: aKey put: aValue
]

{ #category : #accessing }
ZnMimeType >> parameters [
	parameters isNil ifTrue: [ parameters := SmallDictionary new ].
	^ parameters
]

{ #category : #accessing }
ZnMimeType >> parameters: aDictionary [
	parameters := aDictionary
]

{ #category : #copying }
ZnMimeType >> postCopy [
	super postCopy.
	parameters := parameters copy
]

{ #category : #printing }
ZnMimeType >> printOn: stream [
	stream
		nextPutAll: self main;
		nextPut: $/;
		nextPutAll: self sub.
	parameters isNil ifFalse: [ 
		parameters keysAndValuesDo: [ :key :value | 
			stream
				nextPut: $;;
				nextPutAll: key;
				nextPut: $=;
				nextPutAll: value ] ]
]

{ #category : #parameters }
ZnMimeType >> removeParameter: key [
	parameters isNil ifTrue: [ ^ self ].
	^ self parameters removeKey: key ifAbsent: [ nil ]
]

{ #category : #parameters }
ZnMimeType >> setCharSetUTF8 [
	self charSet: 'utf-8'
]

{ #category : #accessing }
ZnMimeType >> sub [
	^ sub
]

{ #category : #accessing }
ZnMimeType >> sub: aString [
	sub := aString asLowercase
]
