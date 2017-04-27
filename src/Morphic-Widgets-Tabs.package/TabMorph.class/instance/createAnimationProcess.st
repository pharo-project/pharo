protocol
createAnimationProcess
	^[ | i newIcon |
			i := 1.
			20 milliSeconds asDelay wait.
			UIManager default defer: [self icon: self class refreshIcon1].
			[ true ] whileTrue:[ 
				i := (i \\ 10)+1.
				100 milliSeconds asDelay wait.
				newIcon := (self class perform: ('refreshIcon', i asString) asSymbol).
				UIManager default defer: [
					self icon: newIcon ] ]] 
					newProcess
						priority: Processor activePriority + 1;
						yourself