testing
testLocalSelectors
	"self run: #testLocalSelectors"

	self assert: self t3 localSelectors size = 3.
	self assert: (self t3 localSelectors includesAll: #(#m31 #m32 #m33 )).
	self assert: (self t3 includesLocalSelector: #m32).
	self deny: (self t3 includesLocalSelector: #inexistantSelector).
	self assert: self t5 localSelectors size = 3.
	self assert: (self t5 localSelectors includes: #m51).
	self assert: (self t5 includesLocalSelector: #m51).
	self deny: (self t5 includesLocalSelector: #m11).
	self t5 removeSelector: #m51.
	self deny: (self t3 includesLocalSelector: #m51).
	self deny: (self t5 includesLocalSelector: #m11).
	self assert: self t5 localSelectors size = 2.
	self t5 compile: 'm52 ^self'.
	self assert: self t5 localSelectors size = 2.
	self assert: (self t5 localSelectors includes: #m52).

	"test that propagated methods do not get in as local methods"
	self t2 compile: 'local2 ^self'.
	self deny: (self t5 includesLocalSelector: #local2).
	self assert: self t5 localSelectors size = 2.
	self assert: (self t5 localSelectors includes: #m52).
	self assert: self c2 localSelectors size = 2.
	self assert: (self c2 localSelectors includesAll: #(#foo #bar ))