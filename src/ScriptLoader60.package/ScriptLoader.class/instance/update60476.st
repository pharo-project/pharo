pharo - updates
update60476
	"self new update60476"
	self withUpdateLog: '19985 SourceFileArray should be encoding independent when read preamble of methods 
	https://pharo.fogbugz.com/f/cases/19985

20003 useExternalPackages mentions Pharo 5.0 although in Pharo 6.0
	https://pharo.fogbugz.com/f/cases/20003

20000 HelpBrowser should open real browser when "WebBrowser" is loaded
	https://pharo.fogbugz.com/f/cases/20000

19998 Settings Syntax highlighting "set to default" crashes the image
	https://pharo.fogbugz.com/f/cases/19998'.
	self loadTogether: self script60476 merge: false.
	
SettingDeclaration initialize.
	self flushCaches.
