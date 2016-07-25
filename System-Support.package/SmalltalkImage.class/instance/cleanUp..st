cleanup
cleanUp: aggressive
	"Clean up. When aggressive is true, this will destroy change sets, etc."
	"Smalltalk cleanUp: false"
	"Smalltalk cleanUp: true"

	^self cleanUp: aggressive except: #()