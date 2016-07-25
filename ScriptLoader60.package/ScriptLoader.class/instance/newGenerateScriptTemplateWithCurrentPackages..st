new script-update format
newGenerateScriptTemplateWithCurrentPackages: aNumber
	"ScriptLoader new generateScriptTemplateWithCurrentPackages"
	"this version change the script to only "
	
	| str withoutScriptLoader |
	str := ReadWriteStream on: (String new: 1000).
	str nextPutAll: 'script', aNumber asString ; cr ; cr ; tab.
	str nextPutAll: '^ '.
	str nextPut: $'.
	withoutScriptLoader := self currentVersionsToBeSaved.
	withoutScriptLoader
		do: [ :each | str nextPutAll: each ; nextPutAll: '.mcz']
		separatedBy: [str nextPut: Character cr].
	str nextPut: $'; nextPut: Character cr.
	str nextPutAll: 'findTokens: String lf , String cr'.
	^ str contents