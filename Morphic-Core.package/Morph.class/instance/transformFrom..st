event handling
transformFrom: uberMorph 
	"Return a transform to be used to map coordinates in a morph above me into my childrens coordinates, or vice-versa. This is used to support scrolling, scaling, and/or rotation. This default implementation just returns my owner's transform or the identity transform if my owner is nil. 
	Note:  This method cannot be used to map into the receiver's coordinate system!"

	(self == uberMorph or: [owner isNil]) ifTrue: [^IdentityTransform new].
	^owner transformFrom: uberMorph