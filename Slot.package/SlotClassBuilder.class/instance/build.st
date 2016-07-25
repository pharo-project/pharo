initialize-release
build
	| oldClass metaChange classChange |
	result := oldClass := installer classAt: name ifAbsent: [ ^ self buildNewClass ].

	self validateSuperChain: oldClass.
	
	metaChange := ClassModification
							modify: oldClass class
							extend: self superMetaclass
							withLayoutType: FixedLayout
							slots: classSlots
							sharedVariables: #()
							sharedPools: ''
							traitComposition: classTraitComposition.
	
	classChange := ClassModification
							modify: oldClass
							extend: superclass
							withLayoutType: self layoutClass
							slots: slots
							sharedVariables: sharedVariables
							sharedPools: sharedPoolsString
							traitComposition: traitComposition.

	self apply: metaChange.
	self apply: classChange.

	comment ifNotNil: [result classComment: comment stamp: commentStamp].

	result instanceSide 
		declareClassVariables: sharedVariables;
		sharing: sharedPoolsString.

	result isMeta ifFalse: [
		installer recategorize: result to: category ].
	
	"call all slots and allow them to modify the class"
	classChange additions do: [ :each | each installingIn: result].
	classChange changes do: [ :each | each changingIn: result ].
	classChange removals do: [ :each | each removingFrom: result].
	result classLayout slots do: [ :each | each layoutChanged: result]. 
	
	
	^ result instanceSide