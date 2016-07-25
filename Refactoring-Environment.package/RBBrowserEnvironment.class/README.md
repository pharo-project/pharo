I am the base class for environments of the refactoring framework.

I define the common interface for all environments.
And I act as a factory for various specialized environments. See my 'environment' protocol.

I am used by different tools to create a 'views' of subsets of the whole system environment to browse or act on (searching/validations/refactoring)

create instances:
RBBrowserEnvironment new forClasses:  Number withAllSubclasses.
RBBrowserEnvironment new forPackageNames: { #Kernel }.

query:
|env|
env := RBBrowserEnvironment new forPackageNames: { #Kernel }.
env referencesTo:#asArray.
-> RBSelectorEnvironment.

browse:
|env|
env := RBBrowserEnvironment new forPackageNames: { #Kernel }.
(Smalltalk tools browser browsedEnvironment: env) open.
