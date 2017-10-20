private
privateAdapterFromModel: aModel withSpec: aSpec
	"apparently when looking at the implementation, it does not return a widget but an adapter so it should be called adapter :)"
	^ SpecInterpreter private_buildWidgetFor: self withSpec: aSpec.