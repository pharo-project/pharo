## Environments

The infrastructure of the refactoring engine defines some environments and operations (and, or,...) over such environments. 
An environment is basically a slice over the system: it can contain for example all the classes of a set of packages. 
The key class is `RBBrowserEnvironment`.
The following shows the class comments of the environments available in Pharo.

### RBBrowserEnvironment

I am the base class for environments of the refactoring framework.

I define the common interface for all environments.
And I act as a factory for various specialized environments. See my 'environment' protocol.

I am used by different tools to create a 'views' of subsets of the whole system environment to browse or act on (searching/validations/refactoring)

#### create instances:

```st
RBBrowserEnvironment new forClasses:  Number withAllSubclasses.
RBBrowserEnvironment new forPackageNames: { #Kernel }.
```

#### query:

```st
|env|
env := RBBrowserEnvironment new forPackageNames: { #Kernel }.
env referencesTo:#asArray.
-> RBSelectorEnvironment.
```

#### browse:

```st
|env|
env := RBBrowserEnvironment new forPackageNames: { #Kernel }.
(Smalltalk tools browser browsedEnvironment: env) open.
```

### RBBrowserEnvironmentWrapper

I am a wrapper around special browser environment subclasses and
the base RBBrowserEnvironment class. I define common methods
for my subclasses to act as a full environment.
no public use.


### RBCategoryEnvironment

I am a RBBrowserEnvironment on a set of category names.
I contains all entities using this category name.
I am more restricted to the exact category name compared
to a package environment.

Example, all Morph subclasses in category Morphic-Base-Menus

```st
(RBBrowserEnvironment new forClasses: Morph withAllSubclasses) forCategories: {#'Morphic-Base-Menus'}
```

### RBClassEnvironment
I am a RBBrowserEnvironment on a set of classes.
I contain all entities of this set.

Example:

```st
(RBBrowserEnvironment new) forClasses: Number withAllSubclasses.
```

### RBClassHierarchyEnvironment

I am a RBBrowserEnvironment on a set of classes of a class hierarchy.

Example:

```st
(RBBrowserEnvironment new) forClass:Morph protocols:{'printing'}.
```

### RBAndEnvironment

I am the combination of two RBEnvironments, a logical AND. That is: 
entity A is in this environment if it is in BOTH environment I am constructed from.

Do not construct instances of me directly, use method #& for two existing environments:
env1 & env2 -> a RBAndEnvironment.

### RBOrEnvironment

I am the combination of two RBEnvironments, a logical OR. That is: 
entity A is in this environment if it is in at least ONE environment I am constructed from.

Do not construct instances of me directly, use method #| for two existing environments:
env1 | env2 -> a RBOrEnvironment.

### RBNotEnvironment

I am the complement of RBEnvironments, a logical NOT. That is:
entity A is in this environment if it is in NOT in the environment I am constructed from.

Do not construct instances of me directly, use method #not for an existing environment:
env1 not -> a RBNotEnvironment.

### RBPackageEnvironment

I am a RBBrowserEnvironment on a set of packages or package names.
I contain all entities are defined in these packages.
(classes and class that have extensions from these packages)

Example:

```st
RBBrowserEnvironment new forPackageNames:{ 'Morphic-Base'}.
```

### RBPragmaEnvironment

I am a RBBrowserEnvironment on a set of Pragmas.
I contain all entities that define methods using this pragmas.
Example:

```st
RBBrowserEnvironment new forPragmas:{ #primitive:}.
```

### RBProtocolEnvironment

I am a RBBrowserEnvironment on a set of protocols of a class.

Example:

```st
RBBrowserEnvironment new forClass:Morph protocols:{'printing'}.
```

### RBSelectorEnvironment

I am a RBBrowserEnvironment for a set of selectors.
Usually I am constructed as a result of a query on another environment:

```st
env referencesTo: #aselector -> a RBSelectorEnvironments.
```

### RBVariableEnvironment

I am a RBBrowserEnvironment for items referring class or instvars.
Constructed by querying existing environments with
referring, reading or writing to the variables of a class.

Example:
```st
RBBrowserEnvironment new instVarWritersTo:#color in: Morph.
-> a RBVariableEnvironment
```
