private
withExampleDo: aBlock
	^ self withExampleDo: aBlock ifAbsent: [ " ignore " ]