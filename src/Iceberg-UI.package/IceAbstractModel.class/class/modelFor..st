instance creation
modelFor: entity
	| model |
	"We are using WeakValueDictionary to keep this sinchronized (in case you have more than 
	 one window open, you want always same model). Now, it has to be cleaned when windows are 
	 no longer around, but associations will be nil... so we cannot use a simple ifAbsentPut:. 
	 Yep, ugly... but effective."
	
	model := self models at: entity name ifAbsent: [ nil ].
	^ model ifNil: [ 
		self compactIfNeeded.
		self models 
			at: entity name
			put: (self basicNew 
				entity: entity; 
				initialize; 
				yourself) ]