An OSPlatform is an abstract representation of a 'OS platform'.
Platforms can be hierarchical, e.g., a "general" platform as superclass and more specific platforms as subclasses as long as the subclasses provide sufficient means to identify themselves.
The original implementation was for Tweak.

Current		holds the current OSPlatform subclass

Architectural considerations:
most platform specific methods that need to be added to the platform class should be in the form of extensions rather then adding them directly to this package. Otherwise the platform class will degenerate very quickly into a dependence hub for all kinds of sub systems.