I am the root class for filters that decide what processors can be loaded in a step. I have no conection whatsoever with the GTFilter hierarchy.

Subclasses need to implement #shouldEnableProcessorsCreateBy: to indicate whether or not a given processor should be enabled.

Subclasses can return a configuration block from #configurationBlockForProcessorsCreatedBy: if they want to customize a certain processor.