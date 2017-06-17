I represent a package, that could be in an IceRepository or in the image.

Public API and Key Messages
- isLoaded tells if the package is currently loaded in the image.

In the future we should add load/unload here.
We could also add commit, but in general is better to commit at the project level and not at the package level.