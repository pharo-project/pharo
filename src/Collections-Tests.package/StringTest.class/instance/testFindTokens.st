tests
testFindTokens

|s |
s := OrderedCollection withAll: #( 'i' 'd toke' 's').
self assert: (('find tokens' findTokens: #($a $f $n)) = s).

s := OrderedCollection withAll: #('éèàôüößäóñí').
self assert: (('éèàôüößäóñíá' findTokens: $á) = s).

s := OrderedCollection withAll: #().
self assert: (('' findTokens: '') = s).

s := OrderedCollection withAll: #('es' ' ' 'his').
self assert: (('test this' findTokens: 't') = s).