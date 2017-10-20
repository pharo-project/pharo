tests - comparing
testCorrectAgainst

| coll |

coll := OrderedCollection new addAll: #('nest' 'temperature' 'text' 'Jupiter' 'denial' 'nest' 'tttt' 'e' 'alpha' 'beautest'); yourself.
self assert: ('test' correctAgainst: coll) asArray = #('beautest' 'nest' 'text' 'nest' 'tttt' 'temperature').
self assert: ('123' correctAgainst: coll) asArray = #('tttt' 'nest' 'text' 'nest').
coll := OrderedCollection new.
self assert: ('test' correctAgainst: coll) asArray = #().