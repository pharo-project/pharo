MCPackage represents a package. It is merely a wrapper on top of a package set or packageInfo.

Strangely enough it does not inherit from MCDefinition.

Its most important method is snapshot which returns a snapshot with all the entities that should be saved. 