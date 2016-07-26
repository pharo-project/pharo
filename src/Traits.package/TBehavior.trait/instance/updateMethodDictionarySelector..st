traits
updateMethodDictionarySelector: aSymbol
	"A method with selector aSymbol in myself or my traitComposition has been changed.
	Do the appropriate update to my methodDict (remove or update method) and
	return all affected selectors of me so that my useres get notified."

	| modifiedSelectors descriptions oldProtocol |
	modifiedSelectors := IdentitySet new.
	descriptions := self traitComposition methodDescriptionsForSelector: aSymbol.
	descriptions do: [:methodDescription | | effectiveMethod selector |
		selector := methodDescription selector.
		(self includesLocalSelector: selector) ifFalse: [
			methodDescription isEmpty
				ifTrue: [
					self removeTraitSelector: selector.
					modifiedSelectors add: selector]
				ifFalse: [
					effectiveMethod := methodDescription effectiveMethod.
					self addTraitSelector: selector withMethod: effectiveMethod.
					
					"If the method was not categorized yet, we categorize it "
					oldProtocol := self organization categoryOfElement: selector.
					(oldProtocol isNil or: [ oldProtocol = Protocol unclassified ]) 
						ifTrue: [ self organization classify: selector under: methodDescription effectiveMethodCategory. ].
					
					modifiedSelectors add: selector]]].
	^modifiedSelectors