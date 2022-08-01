function getAttributes(node) {
  var entries = [];
  for (var i = 0; i < node.attributes.length; i++) {
    let { name, value } = node.attributes.item(i);
    entries.push([name, value]);
  }
  return entries;
}

function walk(treeWalker) {
  var nodes = [];

  function handleNode(node) {
    if (["#comment", "#text"].includes(node.nodeName)) {
      var text = node.textContent;
      if (text) {
        nodes.push({
          type: node.nodeName.slice(1),
          text
        });
      }
    } else {
      var children = walk(treeWalker);
      treeWalker.currentNode = node;
      nodes.push({
        type: "element",
        name: node.localName,
        attributes: getAttributes(node),
        children
      });
    }
  }

  var currentNode = treeWalker.currentNode;
  var firstChild = treeWalker.firstChild();
  if (firstChild) {
    handleNode(firstChild);
  } else {
    return nodes;
  }

  var nextSibling = treeWalker.nextSibling();
  while (nextSibling) {
    handleNode(nextSibling);
    treeWalker.currentNode = nextSibling;
    nextSibling = treeWalker.nextSibling();
  }

  return nodes;
}

exports.parseFromString = elementCtor => attributeCtor => textCtor => commentCtor => input => {
  function mapNode(node) {
    if (node.type == "element") {
      return elementCtor({
        name: node.name,
        attributes: node.attributes.map(([k, v]) => attributeCtor(k)(v)),
        children: node.children.map(mapNode)
      });
    } else {
      var ctor = node.type == "text" ? textCtor : commentCtor;
      return ctor(node.text);
    }
  }

  var doc = new DOMParser().parseFromString(input, "text/html");
  var headNodes = walk(
    doc.createTreeWalker(doc.documentElement.querySelector("head"))
  );
  var bodyNodes = walk(
    doc.createTreeWalker(doc.documentElement.querySelector("body"))
  );

  return [...headNodes, ...bodyNodes].map(node => {
    if (node.type == "element") {
      return mapNode(node);
    } else {
      var ctor = node.type == "text" ? textCtor : commentCtor;
      return ctor(node.text);
    }
  });
};