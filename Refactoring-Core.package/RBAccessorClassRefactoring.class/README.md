I am a refactoring operation for creating accessors for all variables.

Example:
Create accessors for all instance variables:
RBAccessorClassRefactoring model: RBNamespace new className: 'Morph' .

Create accessors for all class instance variables:
RBAccessorClassRefactoring model: RBNamespace new className: 'Morph class' .

If the class already contains that accessor, I will create another one with a numbered suffix.
