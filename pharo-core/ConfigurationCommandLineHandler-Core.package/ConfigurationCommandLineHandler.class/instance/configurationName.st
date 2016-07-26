accessing
configurationName
	^ configurationName ifNil: [
			self hasConfiguration 
				ifFalse: [ "Demeters favourite line :("
					^ self gofer configuration references last name ].
		 configurationName := self argumentAt: 2 ]