I'm the superclass keeping track of the state of the object (added/modified/removed) but according the kind of object the changes apply to. My subclasses KomitClass, KomitDefinition, KomitPackage and KomitMethod wrap MCAddition/MCRemoval/MCDefinition. 

While the hierarchy of MCDefinition is about the kind of change (addition, removal, definition),
the current hierarchy is about the kind of objects changes apply to.

Now it would be interested to see if this hierarchy has been created because of lack of awareness that the hierarchy of MCDefinition is already proposing such facilities. 
