Extension { #name : #ClassDescription }

{ #category : #'*Deprecated12' }
ClassDescription >> addCategory: aString [

	self deprecated: 'Use #addProtocol: instead.' transformWith: '`@rcv addCategory: `@arg' -> '`@rcv addProtocol: `@arg'.
	self addProtocol: aString
]

{ #category : #'*Deprecated12' }
ClassDescription >> addMethodTag: aSymbol [

	self deprecated: 'Use #addProtocol: instead.' transformWith: '`@rcv addMethodTag: `@arg' -> '`@rcv addProtocol: `@arg'.

	self addProtocol: aSymbol
]

{ #category : #'*Deprecated12' }
ClassDescription >> allCategories [

	self deprecated: 'Use #protocolNames instead.' transformWith: '`@rcv allCategories' -> '`@rcv protocolNames'.
	^ self protocolNames
]

{ #category : #'*Deprecated12' }
ClassDescription >> allMethodSelectors [

	self deprecated: 'Use #selectors instead' transformWith: '`@rcv allMethodSelectors' -> '`@rcv selectors'.
	^ self selectors
]

{ #category : #'*Deprecated12' }
ClassDescription >> allProtocolsUpTo: mostGenericClass [
	"Answer a list of all the method protocols of the receiver and all its superclasses, up through mostGenericClass"

	self deprecated: 'This methods will be removed in the next version of Pharo. If you use it, you can inline it.'.

	^ (self allSuperclassesIncluding: mostGenericClass) flatCollectAsSet: [ :aClass | aClass protocolNames ]
]

{ #category : #'*Deprecated12' }
ClassDescription >> categories [

	self deprecated: 'Use #protocolNames instead.' transformWith: '`@rcv categories' -> '`@rcv protocolNames'.
	^ self protocolNames
]

{ #category : #'*Deprecated12' }
ClassDescription >> categoryOfElement: aSelector [

	self
		deprecated: 'Use #protocolNameOfSelector: instead.'
		transformWith: '`@rcv categoryOfElement: `@arg' -> '`@rcv protocolNameOfSelector: `@arg'.
	^ self protocolNameOfSelector: aSelector
]

{ #category : #'*Deprecated12' }
ClassDescription >> categoryOfElement: aSelector ifAbsent: aBlock [

	self
		deprecated: 'Use #protocolNameOfSelector: instead.'
		transformWith: '`@rcv categoryOfElement: `@arg1 ifAbsent: `@arg2' -> '(`@rcv protocolNameOfSelector: `@arg1) ifNil: `@arg2'.
	^ (self protocolNameOfSelector: aSelector) ifNil: aBlock
]

{ #category : #'*Deprecated12' }
ClassDescription >> classComment [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.

	^ self comment
]

{ #category : #'*Deprecated12' }
ClassDescription >> classComment: aString [

	self
		deprecated: 'The organization is not responsible for the class comment anymore. Ask the comment to the class directly.'
		on: '18 April 2023'
		in: 'Pharo-12.0.0+build.183.sha.cf9b49f2a83f05bb9f02ac5831035d1bda76a14a (64 Bit)'.
	self comment: aString
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

	protocol := protocolName ifNil: [ originClass protocolNameOfSelector: selector ].

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

	self copyAll: (aClass selectorsInProtocol: protocolName) from: aClass classified: newProtocolName
]

{ #category : #'*Deprecated12' }
ClassDescription >> listAtCategoryNamed: aName [

	self deprecated: 'Use #selectorsInProtocol: instead.' transformWith: '`@rcv listAtCategoryNamed: `@arg' -> '`@rcv selectorsInProtocol: `@arg'.
	^ self selectorsInProtocol: aName
]

{ #category : #'*Deprecated12' }
ClassDescription >> organization [
	"Answer the instance of ClassOrganizer that represents the organization
	of the messages of the receiver."

	self deprecated: 'ClassOrganization was inlined in ClassDescription. Just use self instead.' transformWith: '`@rcv organization' -> '`@rcv'.
	^ self
]

{ #category : #'*Deprecated12' }
ClassDescription >> organizedClass [

	self deprecated: 'ClassOrganization was inlined and we do not need an organized class anymore.' transformWith: '`@rcv organizedClass' -> '`@rcv'.
	^ self
]

{ #category : #'*Deprecated12' }
ClassDescription >> removeCategory: protocolName [

	self deprecated: 'Use #removeProtocolIfEmpty: instead.' transformWith: '`@rcv removeCategory: `@arg' -> '`@rcv removeProtocolIfEmpty: `@arg'.
	self removeProtocolIfEmpty: protocolName
]

{ #category : #'*Deprecated12' }
ClassDescription >> removeElement: aSelector [
	self deprecated: 'Use #removeFromProtocols: instead.' transformWith: '`@rcv removeElement: `@arg' -> '`@rcv removeFromProtocols: `@arg'. 
	^ self removeFromProtocols: aSelector
]

{ #category : #'*Deprecated12' }
ClassDescription >> removeEmptyCategories [

	self deprecated: 'Use #removeEmptyProtocols instead.' transformWith: '`@rcv removeEmptyCategories' -> '`@rcv removeEmptyProtocols'.
	self removeEmptyProtocols
]

{ #category : #'*Deprecated12' }
ClassDescription >> removeMethodTag: aSymbol [

	self
		deprecated: 'Use #removeProtocolIfEmpty: instead since this previous name is missleading.'
		transformWith: '`@rcv removeMethodTag: `@arg' -> '`@rcv removeProtocolIfEmpty: `@arg'.
	^ self removeProtocolIfEmpty: aSymbol
]

{ #category : #'*Deprecated12' }
ClassDescription >> removeProtocolNamed: protocolName [

	self deprecated: 'Use #removeProtocolIfEmpty: instead' transformWith: '`@rcv removeProtocolNamed: `@arg' -> '`@rcv removeProtocolIfEmpty: `@arg'.
	^ self removeProtocolIfEmpty: protocolName
]

{ #category : #'*Deprecated12' }
ClassDescription >> renameCategory: oldName toBe: newName [

	self
		deprecated: 'Use #renameProtocol:as: instead.'
		transformWith: '`@rcv renameCategory: `@arg1 toBe: `@arg2' -> '`@rcv renameProtocol: `@arg1 as: `@arg2'.
	self renameProtocol: oldName as: newName
]

{ #category : #'*Deprecated12' }
ClassDescription >> selectorsInCategory: aName [

	self deprecated: 'Use #selectorsInProtocol: instead.' transformWith: '`@rcv selectorsInCategory: `@arg' -> '`@rcv selectorsInProtocol: `@arg'.

	^ self selectorsInProtocol: aName
]

{ #category : #'*Deprecated12' }
ClassDescription >> silentlyRenameCategory: oldName toBe: newName [

	self
		deprecated: 'Use #renameProtocol:as: instead. Since Pharo12 it is not possible to not announce a protocol rename anymore because we should always announce those changes.'
		transformWith: '`@rcv silentlyRenameCategory: `@arg1 toBe: `@arg2' -> '`@rcv renameProtocol: `@arg1 as: `@arg2'.
	self renameProtocol: oldName as: newName
]

{ #category : #'*Deprecated12' }
ClassDescription >> subject [

	self deprecated: 'ClassOrganization was inlined and we do not need an organized class anymore.' transformWith: '`@rcv subject' -> '`@rcv'.
	^ self
]

{ #category : #'*Deprecated12' }
ClassDescription >> whichCategoryIncludesSelector: aSelector [

	self deprecated: 'Use #protocolNameOfSelector: instead.' transformWith: '`@rcv whichCategoryIncludeSelector: `@arg' -> '`@rcv protocolNameOfSelector: `@arg'.
	^ self protocolNameOfSelector: aSelector
]
