"
# Fuel

A general-purpose object serialization framework developed in Pharo. Fuel is developed under the MIT license.
Description

- Concrete
We don't aspire to have a dialect-interchange format. This enables us to serialize special objects like contexts, block closures, exceptions, compiled methods and classes. Although there are ports to other dialects, Fuel development is Pharo-centric.
- Flexible
Depending on the context, there could be multiple ways of serializing the same object. For example, a class can be considered either a global or a regular object. In the former case, it will be encoded just its name; in the latter case, the class will be encoded in detail, with its method dictionary, etc.
- Fast
We worry about to have the best performance. We developed a complete benchmark suite to help analyse the performance with diverse sample sets, as well as compare against other serializers. Our pickling algorithm allows outstanding materialization performance, as well as very good serialization performance too.
- Object-Oriented Design

From the beginning it was a constraint to have a good object-oriented design and to do not need any special support from the VM. In addition, Fuel has a complete test suite, with a high coverage. We also worry about writing comments on classes and methods.
Properties

-Binary format.
-Object-Oriented design.
-No special VM-support needed.
-Modular (clear division in packages).
-Can serialize/materialize not only plain objects but also classes, traits, methods, closures, contexts, packages, etc.
-Support for global references.
-Very customizable: ignore certain instance variables, substitute objects by others, pre and post serialization and materialization actions, etc.
-Supports class rename and class reshape.
-Good test coverage (almost 600 unit tests).
-Large suite of benchmarks.

"
Class {
	#name : #ManifestFuelCore,
	#superclass : #PackageManifest,
	#category : #'Fuel-Core-Manifest'
}

{ #category : #'meta data' }
ManifestFuelCore class >> ruleConsistencyCheckRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#Float #serializeOn: #false)) #'2013-02-25T14:50:01.564000001+01:00') )
]

{ #category : #'meta data' }
ManifestFuelCore class >> ruleIfTrueBlocksRuleV1FalsePositive [
^ #(#(#(#RGMethodDefinition #(#FLLightGeneralMapper #visitSubstitution:by:onRecursionDo: #false)) #'2013-02-25T14:50:01.650000001+01:00') )
]
