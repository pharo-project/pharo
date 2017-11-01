Extension { #name : #MCSnapshotTest }

{ #category : #'*MonticelloMocks' }
MCSnapshotTest >> mockClassExtension [

	"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as 'modified'. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. " 
]
