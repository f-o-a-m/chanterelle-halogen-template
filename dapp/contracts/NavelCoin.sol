pragma solidity ^0.4.22;

import "openzeppelin-solidity/contracts/token/ERC20/StandardToken.sol";

contract NavelCoin is StandardToken {

  function NavelCoin(uint totalSupply) public {
    balances[msg.sender] = totalSupply;
    totalSupply_ = totalSupply;
  }
}
