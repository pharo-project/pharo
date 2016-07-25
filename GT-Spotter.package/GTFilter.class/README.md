I am the root class of the hierarchy of filter objects  that can be used by Spotter. 

The main entry point is the #value method that performs and returns the list of filtered items.
Subclasses should return the list of filtered elements in sorted order based on the filtering criteria.