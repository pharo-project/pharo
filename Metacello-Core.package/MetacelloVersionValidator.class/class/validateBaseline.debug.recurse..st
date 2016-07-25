instance creation
validateBaseline: baselineClass debug: debugList recurse: aBool
  ^ (self new
    configurationClass: baselineClass;
    debug: debugList;
    recurse: aBool;
    yourself) validateBaselineOf