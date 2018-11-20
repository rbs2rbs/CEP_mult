// the event handler listens to shiny for messages send by handler1
// if it receives a message, call the callback function doAwesomething and pass the message
//Shiny.addCustomMessageHandler("handler1", doAwesomeThing );

// this function is called by the handler, which passes the message
//function doAwesomeThing(message){
  
  // show the messsage as an alert
  //alert(message);
//}


$(document).ready(function(){
  $("#drop").addClass("hide")
  Shiny.addCustomMessageHandler("dropS", dropGraficoS );
  function dropGraficoS(x){
    console.log([x.curveNumber,"s"]);
    Shiny.onInputChange("variavel",[x.curveNumber,"s"]);
    $("#drop").click();
  }
  Shiny.addCustomMessageHandler("dropX", dropGraficoX );
  function dropGraficoX(x){
    console.log([x.curveNumber,"x"]);
    Shiny.onInputChange("variavel",[x.curveNumber,"x"]);
    $("#drop").click();
  }
})