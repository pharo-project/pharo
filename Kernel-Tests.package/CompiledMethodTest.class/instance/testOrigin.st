tests - accessing
testOrigin
	| regularMethod methodFromTrait aliasedMethod |

	"Regular method"
	regularMethod := Behavior>>#name.
	
	"Method from a trait without alias "
	methodFromTrait := Behavior>>#addToComposition:.

	"Method from a trait with an alias"
	aliasedMethod := MOPTestClassD >>#c3.

	self assert: regularMethod origin == regularMethod originMethod methodClass.
	self assert: methodFromTrait origin == methodFromTrait originMethod methodClass.
	self assert: aliasedMethod origin == aliasedMethod originMethod methodClass.