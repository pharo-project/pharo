tests
testFindBetweenSubstrings
 
self assert: ('éèà ôüößäó ñíá' findBetweenSubstrings: #(' ')) asArray = #('éèà' 'ôüößäó' 'ñíá').
self assert: ('ieaoftaaka' findBetweenSubstrings: #($a)) asArray = #('ie' 'oft' 'k').
self assert: ('this is an issue is it not' findBetweenSubstrings: #('is' 'not')) asArray 
                                               = #('th' ' ' ' an ' 'sue ' ' it ').