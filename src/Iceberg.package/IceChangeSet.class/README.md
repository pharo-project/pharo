I represent a (maybe unitary) set of changes to be handled by the Iceberg framework. I am the top class of a composite pattern.

A change should be defined as
1. the difference between two versions of the code (currently two MCSnapthots for an IcePackageChangeSet)
2. a filter from a bigger change set (for example we can create a class change set which is a subset of a package change set, all other subclasses from IceStructuralChangeSet).
3. an explicit set of changes (see IceSimpleChangeSet).

Public API and Key Messages
- elements: returns a set of change sets which are my children, for example the change set related to a package contains the change sets for each class in the package.

- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	elements:		<Object>


    Implementation Points