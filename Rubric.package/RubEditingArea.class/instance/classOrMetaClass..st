as yet unclassified
classOrMetaClass: aBehavior
	"set the classOrMetaClass in the receiver's styler to aBehavior"
	self shoutStyler ifNotNil: [ :styler | styler classOrMetaClass: aBehavior]