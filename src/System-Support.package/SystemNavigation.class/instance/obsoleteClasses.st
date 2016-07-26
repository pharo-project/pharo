query
obsoleteClasses
	"SystemNavigation new obsoleteClasses inspect"

	"NOTE:  Also try inspecting comments below"

	| obs |
	obs := OrderedCollection new.
	Smalltalk garbageCollect.
	Metaclass
		allInstancesDo: [ :m | 
			| c |
			c := m soleInstance.
			(c notNil and: [ 'AnOb*' match: c name asString ])
				ifTrue: [ obs add: c ] ].
	^ obs asArray	"Likely in a ClassDict or Pool...
(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray
"	"Obsolete class refs or super pointer in last lit of a method...
| n l found |
Smalltalk browseAllSelect:
	[:m | found := false.
	1 to: m numLiterals do:
		[:i | (((l := m literalAt: i) isMemberOf: Association)
				and: [(l value isKindOf: Behavior)
				and: ['AnOb*' match: l value name]])
			ifTrue: [found := true]].
	found]
"