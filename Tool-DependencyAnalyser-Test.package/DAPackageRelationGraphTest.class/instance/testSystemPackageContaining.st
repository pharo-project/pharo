tests
testSystemPackageContaining
	self assert: ( (aPackageRelationGraph systemPackageContaining: Collection) packageName = 'Collections-Abstract').
	self assert: ( (aPackageRelationGraph systemPackageContaining: DAPackageRelationGraph) packageName =  #'Tool-DependencyAnalyser')