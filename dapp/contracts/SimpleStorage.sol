contract SimpleStorage {

  uint public count;

  event CountSet(uint newCount);
  
  function SimpleStorage(uint _count) {
    count = _count;
  }

  function setCount(uint _newCount) {
    count = _newCount;
    emit CountSet(_newCount);
  }

}