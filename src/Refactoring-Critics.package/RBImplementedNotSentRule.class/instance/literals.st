cache
literals
	"return all 'message sends' in the systed and cache them"

	literals ifNil: [
		literals := IdentitySet new.

		Smalltalk specialSelectors do: [:sel |
			sel isSymbol ifTrue: [ literals add: sel ] ].
		result rootEnvironment methodsDo: [ :meth |
			meth literals do: [ :lit | (meth hasLiteral: lit) ifTrue: [self addLiteral: lit]]]].
			"here we do a second check as #literals returns all literals even the one storing
			 method name"
	^ literals 