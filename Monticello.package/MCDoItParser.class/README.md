A MCDoItParser is a simple 'parser' which understand the addDefinitionsTo: message. Each parser can use the source
and add definitions to the list of entities that is passed to them.

MCDoitParser invokes automatically its subclasses to parse the correct source.

Each Doit entities (entities which are defined as doits) extend this entry point to add specific behavior.