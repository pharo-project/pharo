accessing
asSelectorEnvironment
	| s |
	s := RBSelectorEnvironment new.
	s label: self label.
	environmentDictionaries do: [:each | | env |
			env := each asSelectorEnvironment. 
			env classesDo: [:cls | env selectorsForClass: cls do:  [:sel | s addClass: cls selector: sel]]].
	^ s
	  