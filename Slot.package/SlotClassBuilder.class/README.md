I'm responsible for building and verifying new classes. 
The update and modification/installation of classes are managed by installers (AbstractClassInstallation).

The class builder is responsible for the structural part of modifying a class or creating a new class. It relies on the installer to fetch the old version of the class. It then uses the class modification model to compute the method modification and instance modification models. It then validates if these changes are semantically sound.

Example:
	see PharoClassInstaller