as yet unclassified
check: aClass forCriticsDo: aCriticBlock ifNone: alternativeBlock
	(self basicCheck: aClass)
		ifTrue:  [ aCriticBlock cull: (
			(ReMissingMethodCritic
				for: aClass
				by: self
				class: aClass class
				selector: #gtExample) beShouldBeImplemented )  ]
		ifFalse: alternativeBlock 