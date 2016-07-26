I am a refactoring for moving the definition of a variable to the block/scope where it is used.

For a method temporary variable declared but not initialized in the method scope and only used within a block, the definition can be moved to the block using this variable.