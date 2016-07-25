private
basicImportClass: aClass
	"Add the class definition and all the selectors which are not extensions of other packages. If the class had an extension to us, then all methods in that extension are moved to 'as yet unclassified' and the extension protocol is deleted (should I use silently or not?)."
	
	"Note: Protocol defaultName is #'as yet unclassified'"

	"Question: should we check that for each extension, there is a real package behind or not?"

	| protocols |
	self addClassDefinition: aClass.
	aClass organization categories
		do: [ :each | 
			(self isYourClassExtension: each)
				ifTrue: [ aClass organization renameCategory: each toBe: Protocol defaultName ] ].
	protocols := aClass organization categories reject: [ :each | each first = $* ].
	protocols do: [ :each | self importProtocol: each forClass: aClass ]