api-configuration
createBaselineOfMethod: selector inCategory: category
  methodSpec := MetacelloBaselineOfMethodSpec new
    project: project;
    selector: selector;
    category: category;
    yourself