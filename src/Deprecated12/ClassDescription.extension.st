Extension { #name : #ClassDescription }

{ #category : #'*Deprecated12' }
ClassDescription >> allProtocolsUpTo: mostGenericClass [
	"Answer a list of all the method protocols of the receiver and all its superclasses, up through mostGenericClass"

	self deprecated: 'This methods will be removed in the next version of Pharo. If you use it, you can inline it.'.

	^ (self allSuperclassesIncluding: mostGenericClass) flatCollectAsSet: [ :aClass | aClass organization protocolNames ]
]

{ #category : #'*Deprecated12' }
ClassDescription >> classesThatImplementAllOf: selectorSet [
	"Return an array of any classes that implement all the messages in selectorSet."

	| found remaining |
	self deprecated: 'This method will be removed in the next version of Pharo.'.
	found := OrderedCollection new.
	selectorSet do: [ :sel | (self includesSelector: sel) ifTrue: [ found add: sel ] ].
	found ifEmpty: [ ^ self subclasses inject: Array new into: [ :subsThatDo :sub | subsThatDo , (sub classesThatImplementAllOf: selectorSet) ] ] ifNotEmpty: [
		remaining := selectorSet copyWithoutAll: found.
		remaining isEmpty ifTrue: [ ^ Array with: self ].
		^ self subclasses inject: Array new into: [ :subsThatDo :sub | subsThatDo , (sub classesThatImplementAllOf: remaining) ] ]
]

{ #category : #'*Deprecated12' }
ClassDescription >> copy: selector from: originClass [
	"Install a copy of originClass>>selector in myself as un unclassified method."

	self deprecated: 'This method will be removed in the next version of Pharo.'.
	self copy: selector from: originClass classified: nil
]

{ #category : #'*Deprecated12' }
ClassDescription >> copy: selector from: originClass classified: protocolName [
	"Install a copy of originClass>>selector in myself under the specified protocol."

	| sourceCode protocol |
	"Useful when modifying an existing class"
	self deprecated: 'This method will be removed in the next version of Pharo.'.
	sourceCode := originClass sourceCodeAt: selector.

	sourceCode ifNil: [ ^ self ].

	protocol := protocolName ifNil: [ originClass organization protocolNameOfElement: selector ].

	(self includesLocalSelector: selector) ifTrue: [
		sourceCode asString = (self sourceCodeAt: selector) asString ifFalse: [ self error: self name , ' ' , selector , ' will be redefined if you proceed.' ] ].

	self compile: sourceCode classified: protocol
]

{ #category : #'*Deprecated12' }
ClassDescription >> copyAll: selectors from: originClass [
	"Install a copy of all selectors from originClass in myself as unclassified methods."

	self deprecated: 'This method will be removed in the next version of Pharo.'.

	self copyAll: selectors from: originClass classified: nil
]

{ #category : #'*Deprecated12' }
ClassDescription >> copyAll: selectors from: originClass classified: protocolName [
	"Install a copy of all selectors from originClass in myself under the specified protocol."

	self deprecated: 'This will be removed in the next version of Pharo.'.

	selectors do: [ :selector | (originClass includesLocalSelector: selector) ifTrue: [ self copy: selector from: originClass classified: protocolName ] ]
]

{ #category : #'*Deprecated12' }
ClassDescription >> copyAllCategoriesFrom: originClass [
	"Install a copy of each methods of originClass in myself under the same protocol."

	self deprecated: 'Use #copyAllMethodsFrom: instead.' transformWith: '`@rcv copyAllCategoriesFrom: `@arg' -> '`@rcv copyAllMethodsFrom: `@arg'.

	self copyAllMethodsFrom: originClass
]

{ #category : #'*Deprecated12' }
ClassDescription >> copyCategory: protocolName from: originClass [
	"Install all methods in the specified protocol of originClass in myself keeping the right protocol."

	self deprecated: 'This method will be removed in the next version of Pharo.'.

	self copyCategory: protocolName from: originClass classified: protocolName
]

{ #category : #'*Deprecated12' }
ClassDescription >> copyCategory: protocolName from: aClass classified: newProtocolName [
	"Move all methods in the protocol 'protocolName' of aClass into a protocol named 'newProtocolName'"

	self deprecated: 'This method will be removed in the next version of Pharo.'.

	self copyAll: (aClass organization methodSelectorsInProtocol: protocolName) from: aClass classified: newProtocolName
]

{ #category : #'*Deprecated12' }
ClassDescription >> removeMethodTag: aSymbol [

	self
		deprecated: 'Use #removeProtocolIfEmpty: instead since this previous name is missleading.'
		transformWith: '`@rcv removeMethodTag: `@arg' -> '`@rcv removeProtocolIfEmpty: `@arg'.
	^ self removeProtocolIfEmpty: aSymbol
]
