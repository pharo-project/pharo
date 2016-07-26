search
testPerfect: aSelector
	"Try this selector! Return true if it answers every example perfectly.  Take the args in the order they are.  Do not permute them.  Survive errors.  later cache arg lists."

	| sz argList val rec activeSel perform |

	perform := aSelector beginsWith: 'perform:'.
	sz := argMap size.
	1 to: thisData size do: [:ii | "each example set of args"
		argList := (thisData at: ii) copyFrom: 2 to: sz.
		perform
			ifFalse: [activeSel := aSelector]
			ifTrue: [activeSel := argList first.	"what will be performed"
				((Approved includes: activeSel) or: [AddAndRemove includes: activeSel])
					ifFalse: [^ false].	"not approved"
				aSelector == #perform:withArguments: 
					ifTrue: [activeSel numArgs = (argList at: 2) basicSize "avoid error" 
								ifFalse: [^ false]]
					ifFalse: [activeSel numArgs = (aSelector numArgs - 1) 
								ifFalse: [^ false]]].
		1 to: sz do: [:num | 
			(Blocks includes: (Array with: activeSel with: num)) ifTrue: [
				(argList at: num) isBlock ifFalse: [^ false]]].
		rec := (AddAndRemove includes: activeSel) 
				ifTrue: [(thisData at: ii) first isSymbol ifTrue: [^ false].
						"vulnerable to modification"
					(thisData at: ii) first copyTwoLevel] 	"protect from damage"
				ifFalse: [(thisData at: ii) first].
		val := [[rec perform: aSelector withArguments: argList] 
					ifError: [:aString :aReceiver | 
							^ false]] on: Deprecation do: [:depr | 
							"We do not want to list deprecated methods" 
							^false.].
		((answers at: ii) closeTo: val) ifFalse: [^ false].
		].
	^ true