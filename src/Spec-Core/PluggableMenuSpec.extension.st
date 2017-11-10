Extension { #name : #PluggableMenuSpec }

{ #category : #'*Spec-Core' }
PluggableMenuSpec >> addToMenuModel: aMenuModel [
	| groups tmp |
	
	groups := OrderedCollection new.
	self label ifNotNil: [ aMenuModel title: self label ]. "only change the title when a new one is specified"
	
	tmp := OrderedCollection new.
	self items do: [ :each | 
		tmp add: each.
		each separator
			ifTrue: [ 
				groups add: tmp copy.
				tmp removeAll ] ].
	tmp ifNotEmpty: [ groups add: tmp copy ].
	
	groups do: [ :each | aMenuModel 
		addGroup: [ :group | each do: [ :spec | 
			group addItem: [ :item | item fromSpec: spec ] ] ] ]
]
