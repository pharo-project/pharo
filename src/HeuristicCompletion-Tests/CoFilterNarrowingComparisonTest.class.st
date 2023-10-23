Class {
	#name : 'CoFilterNarrowingComparisonTest',
	#superclass : 'TestCase',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testCaseInsensitiveFilterIsNarrowWhenTheInternalAreNarrowed [

	| caseSensitive caseSensitive2 |

	caseSensitive := CoCaseInsensitiveBeginsWithFilter filterString: 'ab'.
	caseSensitive2 := CoCaseInsensitiveBeginsWithFilter filterString: 'abc'.

	self deny: (caseSensitive isMoreNarrowThan: caseSensitive2).
	self assert: (caseSensitive2 isMoreNarrowThan: caseSensitive)
]

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testCaseInsensitiveFilterIsNotNarrowThanNegated [

	| caseInsensitive negated |

	caseInsensitive := CoCaseInsensitiveBeginsWithFilter filterString: 'ab'.
	negated := caseInsensitive negated.

	self deny: (caseInsensitive isMoreNarrowThan: negated)
]

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testCaseInsensitiveFilterIsNotNarrowWhenTheInternalAreNotNarrowed [

	| caseSensitive caseSensitive2 |

	caseSensitive := CoCaseInsensitiveBeginsWithFilter filterString: 'ab'.
	caseSensitive2 := CoCaseInsensitiveBeginsWithFilter filterString: 'ax'.

	self deny: (caseSensitive isMoreNarrowThan: caseSensitive2).
	self deny: (caseSensitive2 isMoreNarrowThan: caseSensitive)
]

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testCaseSensitiveFilterIsNarrowWhenTheInternalAreNarrowed [

	| caseSensitive caseSensitive2 |

	caseSensitive := CoCaseSensitiveBeginsWithFilter filterString: 'ab'.
	caseSensitive2 := CoCaseSensitiveBeginsWithFilter filterString: 'abc'.

	self deny: (caseSensitive isMoreNarrowThan: caseSensitive2).
	self assert: (caseSensitive2 isMoreNarrowThan: caseSensitive)
]

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testCaseSensitiveFilterIsNotNarrowThanNegated [

	| caseSensitive negated |

	caseSensitive := CoCaseSensitiveBeginsWithFilter filterString: 'ab'.
	negated := caseSensitive negated.

	self deny: (caseSensitive isMoreNarrowThan: negated)
]

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testCaseSensitiveFilterIsNotNarrowWhenTheInternalAreNotNarrowed [

	| caseSensitive caseSensitive2 |

	caseSensitive := CoCaseSensitiveBeginsWithFilter filterString: 'ab'.
	caseSensitive2 := CoCaseSensitiveBeginsWithFilter filterString: 'ax'.

	self deny: (caseSensitive isMoreNarrowThan: caseSensitive2).
	self deny: (caseSensitive2 isMoreNarrowThan: caseSensitive)
]

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testNegatedFilterIsNarrowWhenTheInternalAreNarrowed [

	| caseSensitive negated caseSensitive2 negated2 |

	caseSensitive := CoCaseSensitiveBeginsWithFilter filterString: 'ab'.
	caseSensitive2 := CoCaseSensitiveBeginsWithFilter filterString: 'abc'.

	self deny: (caseSensitive isMoreNarrowThan: caseSensitive2).
	self assert: (caseSensitive2 isMoreNarrowThan: caseSensitive).

	negated := caseSensitive negated.
	negated2 := caseSensitive2 negated.

	self deny: (negated isMoreNarrowThan: negated2).
	self assert: (negated2 isMoreNarrowThan: negated)
]

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testNegatedFilterIsNotNarrowThanANegatedWithANonNarrowed [

	| caseSensitive negated caseSensitive2 negated2 |

	caseSensitive := CoCaseSensitiveBeginsWithFilter filterString: 'ab'.
	caseSensitive2 := CoCaseSensitiveBeginsWithFilter filterString: 'ax'.

	self deny: (caseSensitive isMoreNarrowThan: caseSensitive2).
	self deny: (caseSensitive2 isMoreNarrowThan: caseSensitive).

	negated := caseSensitive negated.
	negated2 := caseSensitive2 negated.

	self deny: (negated isMoreNarrowThan: negated2).
	self deny: (negated2 isMoreNarrowThan: negated)
]

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testNegatedFilterIsNotNarrowThanCaseInsensitive [

	| caseInsensitive negated |

	caseInsensitive := CoCaseInsensitiveBeginsWithFilter filterString: 'ab'.
	negated := caseInsensitive negated.

	self deny: (negated isMoreNarrowThan: caseInsensitive)
]

{ #category : 'tests' }
CoFilterNarrowingComparisonTest >> testNegatedFilterIsNotNarrowThanCaseSensitive [

	| caseSensitive negated |

	caseSensitive := CoCaseSensitiveBeginsWithFilter filterString: 'ab'.
	negated := caseSensitive negated.

	self deny: (negated isMoreNarrowThan: caseSensitive)
]
