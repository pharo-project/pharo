authors
authors
	"Returns a bag representing the author frequency based on the latest version of the methods of the receiver."

	^(self methods, self theMetaClass methods) collect: [ :each | each author ] as: Bag