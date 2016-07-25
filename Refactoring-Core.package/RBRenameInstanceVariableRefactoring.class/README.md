I am a refactoring for rename instance variables.

I rename the instance variable in the class definition, in all methods refering to this variable and rename the old accessors.

My precondition verifies that the new variable is valid and not yet used in the whole class hierarchy.