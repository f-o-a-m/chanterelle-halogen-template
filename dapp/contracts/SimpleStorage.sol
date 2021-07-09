contract SimpleStorage {

  uint public count;

  event CountSet(uint newCount);
  
  constructor (uint _count) public {
    count = _count;
  }

  function setCount(uint _newCount) public {
    count = _newCount;
    emit CountSet(_newCount);
  }

}