tests - instance creation
testCreationWithRows

	"
	> grid22WithPointLocationCreatedWithRows
	(
	(1@1) (2@1)
	(1@2) (2@2)
	)
	"
	self assert: (grid22WithPointLocationCreatedWithRows atRow: 1 atColumn: 1) equals: 1@1.
	self assert: (grid22WithPointLocationCreatedWithRows atRow: 2 atColumn: 2) equals: 2@2.
	self assert: (grid22WithPointLocationCreatedWithRows atRow: 2 atColumn: 1) equals: 1@2.
	self assert: (grid22WithPointLocationCreatedWithRows atRow: 1 atColumn: 2) equals: 2@1.
	