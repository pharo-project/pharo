Extension { #name : #String }

{ #category : #'*EpiceaBrowsers' }
String >> asInstanceSideBehaviorName [

	^ (self splitOn: Character space) first
]
