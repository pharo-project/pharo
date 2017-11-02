Extension { #name : #RPackageTag }

{ #category : #'*Nautilus' }
RPackageTag >> asNautilusItemPath [
	^ 	Array 
			with: self package
			with: self.

]

{ #category : #'*Nautilus' }
RPackageTag >> asNautilusNodeWithModel: anObject [
	^ PackageTreeTagNodeModel with: self model: anObject.
]

{ #category : #'*Nautilus' }
RPackageTag >> asNautilusSelection [ 
	^ PackageTreeTagSelection tag: self
]

{ #category : #'*Nautilus' }
RPackageTag >> withClassesIn: aRBBrowserEnvironment createHierarchySorter: aClass [ 
	^ self asNautilusSelection withClassesIn: aRBBrowserEnvironment createHierarchySorter: aClass
]
