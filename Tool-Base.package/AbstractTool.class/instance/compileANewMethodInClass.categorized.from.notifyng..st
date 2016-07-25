method
compileANewMethodInClass: aClass categorized: aCategory from: aString notifyng: aController

	| selector existingMethod targetClass |
	
	targetClass := aClass.	
	"try to guess the selector form the first line XXX hack"
	selector := aString lines first findSelector.
	
	"Warn the user if a Trait method would be overridden"
	(aClass includesSelector: selector) ifTrue: [
		existingMethod := aClass methodNamed: selector.
		existingMethod isFromTrait ifTrue: [
			targetClass := UIManager default 
				chooseFrom:  {'Create copy in ', aClass name. 
								'Compile Trait method in ', existingMethod origin name}
				values: { aClass. existingMethod origin}
				title: 'Where do you want to compile this trait method?'.
			targetClass ifNil: [ ^ nil ]]].
	
	"Run the normal compilation either on aClass or the Trait"
	^ targetClass compile: aString classified: aCategory notifying: aController.