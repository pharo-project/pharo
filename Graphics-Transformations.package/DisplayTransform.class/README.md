This class represents a base for generic transformations of 2D points between different coordinate systems (including scaling and rotation). The transformations map objects between one coordinate system and another where it is assumed that a nested hierarchy of transformations can be defined.

It is assumed that transformations deal with Integer points. All transformations should return Integer coordinates (even though float points may be passed in as argument).

Compositions of transformations MUST work in the following order. A 'global' transformation (the argument in #composedWithGlobal:) is defined as a transformation that takes place between the receiver (the 'local') transformation and any 'global' point computations, whereas a 'local' transformation (e.g., the argument in #composedWithLocal:) takes place between the receiver ('global') and any 'local' points. For the transformation methods this means that combining a global and a local transformation will result in the following order:

		globalPointToLocal: globalPoint
			"globalPoint -> globalTransform -> localTransform -> locaPoint"
			^localTransform globalPointToLocal:
				(globalTransform globalPointToLocal: globalPoint)

		localPointToGlobal: localPoint
			"localPoint -> localTransform -> globalTransform -> globalPoint"
			^globalTransform localPointToGlobal:
				(localTransform localPointToGlobal: localPoint)

