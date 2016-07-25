connectors-copying
veryDeepInner: deepCopier 
	"Copy all of my instance variables.
	Some otherProperties need to be not copied at all, but shared. Their names are given by copyWeakly.
	Some otherProperties should not be copied or shared. Their names are given by propertyNamesNotCopied.
	This is special code for the dictionary. See DeepCopier, and veryDeepFixupWith:."

	| namesOfWeaklyCopiedProperties weaklyCopiedValues |
	super veryDeepInner: deepCopier.
	locked := locked veryDeepCopyWith: deepCopier.
	visible := visible veryDeepCopyWith: deepCopier.
	sticky := sticky veryDeepCopyWith: deepCopier.
	balloonText := balloonText veryDeepCopyWith: deepCopier.
	externalName := externalName veryDeepCopyWith: deepCopier.
	eventHandler := eventHandler veryDeepCopyWith: deepCopier. 	"has its own restrictions"
	
	fillStyle := fillStyle veryDeepCopyWith: deepCopier.
	layoutPolicy := layoutPolicy veryDeepCopyWith: deepCopier.
	layoutFrame := layoutFrame veryDeepCopyWith: deepCopier.
	layoutProperties := layoutProperties veryDeepCopyWith: deepCopier.
	borderStyle := borderStyle  veryDeepCopyWith: deepCopier.
	cornerStyle := cornerStyle veryDeepCopyWith: deepCopier.
	actionMap := actionMap veryDeepCopyWith: deepCopier.
	clipSubmorphs := clipSubmorphs veryDeepCopyWith: deepCopier.

	otherProperties ifNil: [ ^self ].

	otherProperties := otherProperties copy.
	self propertyNamesNotCopied do: [ :propName | otherProperties removeKey: propName ifAbsent: [] ].

	namesOfWeaklyCopiedProperties := self copyWeakly.
	weaklyCopiedValues := namesOfWeaklyCopiedProperties collect: [  :propName | otherProperties removeKey: propName ifAbsent: [] ].

	"Now copy all the others."
	otherProperties := otherProperties veryDeepCopyWith: deepCopier.

	"And replace the weak ones."
	namesOfWeaklyCopiedProperties with: weaklyCopiedValues do: [ :name :value | value ifNotNil: [ otherProperties at: name put: value ]].
