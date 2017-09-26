tests - accessing
testOrigin
	| regularMethod methodFromTrait aliasedMethod |

	"Regular method"
	regularMethod := Behavior>>#name.
	
	"Method from a trait without alias "
	methodFromTrait := Behavior>>#addToComposition:.

	self assert: regularMethod origin == regularMethod originMethod methodClass.
	self assert: methodFromTrait origin == methodFromTrait originMethod methodClass.
