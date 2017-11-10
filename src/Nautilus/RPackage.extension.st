Extension { #name : #RPackage }

{ #category : #'*Nautilus' }
RPackage >> asNautilusItemPath [
	^ Array with: self
]

{ #category : #'*Nautilus' }
RPackage >> asNautilusNodeWithModel: anObject [
	^ PackageTreePackageNodeModel with: self model: anObject.
]

{ #category : #'*Nautilus' }
RPackage >> asNautilusSelection [ 
	^ PackageTreePackageSelection package: self
]

{ #category : #'*Nautilus' }
RPackage >> withClassesIn: anEnvironment createHierarchySorter: aHierarchClass [
	^ self asNautilusSelection withClassesIn: anEnvironment createHierarchySorter: aHierarchClass
]
