process specific
installEnvIntoForked: newProcess
	env ifNil: [ ^ self ].
	InheritablePSKeys ifNil: [ ^self ].

	"InheritablePSKeys includes indices of all inheritable variables"
	1 to: InheritablePSKeys size do: [ :i | | varIndex varValue |
		varIndex := InheritablePSKeys at: i.
		(varIndex <= env size) "if new variable was installed into system existed processes env can not have room for it"
			ifTrue: [ 
				varValue := env at: varIndex.
				varValue ifNotNil: [ (PSKeys at: varIndex) installValue: varValue intoForked: newProcess from: self ] ]]