mappings
mappingToIdentifiers
	"Return a dictionay mapping from encoding specifications to a list of encoding names."
	
	^ Dictionary newFromPairs: #(
		"#asciiMapping #('ascii')"
		"#iso88591Mapping #('iso88591' 'latin1')"
		#iso88592Mapping #('iso88592' 'latin2') 
		#iso88593Mapping #('iso88593' 'latin3') 
		#iso88594Mapping #('iso88594' 'latin4') 
		#iso88595Mapping #('iso88595' 'cyrillic') 
		#iso88596Mapping #('iso88596' 'arabic') 
		#iso88597Mapping #('iso88597' 'greek') 
		#iso88598Mapping #('iso88598' 'hebrew') 
		#iso885910Mapping #('iso885910' 'latin6') 
		#iso885913Mapping #('iso885913') 
		#iso885914Mapping #('iso885914') 
		#iso885915Mapping #('iso885915') 
		#iso885916Mapping #('iso885916') 
		#cp1250Mapping #('cp1250' 'windows1250' 'xcp1250') 
		#cp1251Mapping #('cp1251' 'windows1251' 'xcp1251') 
		#cp1252Mapping #('cp1252' 'windows1252' 'xcp1252' 'ibm819')
		#cp1253Mapping #('cp1253' 'windows1253' 'xcp1253')
		#cp1254Mapping #('cp1254' 'windows1254' 'xcp1254' 'iso88599' 'latin5')
		#cp1255Mapping #('cp1255' 'windows1255' 'xcp1255')
		#cp1256Mapping #('cp1256' 'windows1256' 'xcp1256')
		#cp1257Mapping #('cp1257' 'windows1257' 'xcp1257')
		#cp1258Mapping #('cp1258' 'windows1258' 'xcp1258')
		#cp850Mapping #('cp850' 'ibm850' 'oem850' 'doslatin1')
		#cp866Mapping #('cp866' 'ibm866')
		#cp874Mapping #('cp874' 'iso885911' 'windows874' 'dos874')
		#koi8rMapping #('koi8r' 'koi8')
		#koi8uMapping #('koi8u')
		#macRomanMapping #('macroman' 'xmacroman' 'mac' 'macintosh')
		#macCyrillicMapping #('maccyrillic' 'xmaccyrillic') )