"
Unit test for ZnStaticFileDecoratorDelegate
"
Class {
	#name : #ZnStaticFileDecoratorDelegateTest,
	#superclass : #TestCase,
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #tests }
ZnStaticFileDecoratorDelegateTest >> testServeStaticFile [
	|  delegate handled response file |
	handled := false.

	file := (FileSystem memory / 'tmp/some.html') ensureCreateFile; yourself.
	file writeStream nextPutAll: 'test123'; close .

	delegate := ZnStaticFileDecoratorDelegate
		decorate: (ZnValueDelegate new object: [ :req | handled := true ])
		servingFilesFrom: (file fileSystem root / 'tmp/') asFileReference.

	"request matches file in filesystem so it returns the file content"
	response := delegate handleRequest: (ZnRequest get: '/some.html' asZnUrl).
	self assert: response contents asString equals: 'test123'.
	self deny: handled.

	"request does not match on the filesystem an the value delegate is called"
	response := delegate handleRequest: (ZnRequest get: '/other.html' asZnUrl).
	self assert: handled
]
