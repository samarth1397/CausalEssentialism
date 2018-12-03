var debugging = 0; 
var conds = []; instructionChecks = []; pfInstructionChecks = []; testwords = [];
var startPauseVal = 250; minusPauseVal = 15; minPauseVal = 10; masklen = 200;
var longpauselen = 8000; shortpauselen = 200; currpauselen = startPauseVal; 
var ind = 0; itemCorr = 0;
var exp_data = {};    hideElements();
var numeric = [
];
var responses=[];
var responses_dict={};
var imageFolders=[];
var imageFolderFeatureDict={};
var randomImageNumbers=[];
var imageCategoryNumberDict={};
var randomnumber=0;
var instructionAttempts=0;
var analogyAttempts=0;
var subjectID="";
var terminated=false;
var extraResponse={};
var object1={}
var object2={}

// ********** START: this function runs automatically when the page is loaded
$(document).ready(function () {

	var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    for( var i=0; i < 9; i++ ) {
      subjectID += possible.charAt(Math.floor(Math.random() * possible.length));
    }
    exp_data['subjectID']=subjectID;
    
    hideElements();
    // loadAnalogy();
    showConsent();

});

// Load analogy question

function loadAnalogy(){
	$('#instructions').show();
	$('#instructions').load('html/analogyTest.html');
    $('#next').show();
    $('#next').click(validateAnalogy); 
}

// validate analogy

// ********** VALIDATEENGLISHSEQUENCE: check to make sure they report the words right
function validateAnalogy() {
  
    englishwords = $('#analogy').serializeArray();
    var ok = true;
  	var empty=false;
  	
   	if(englishwords[0].value === ""){
   		ok = false;
		empty=true;
	
	}else{
     	ans=englishwords[0].value;
     	ans=ans.toLowerCase();

	    if(!(ans=='moon'|| ans=='star'||ans=="stars")) {
	        ok = false;    
	    }
     }



    // goes to next section
    if (!ok) {
    	// console.log(empty)
    	if (empty==true){
    		alert('Please answer this question to proceed.');
    		
    	}
    	else{
    		alert('Incorrect answer. Experiment terminated.')
    		terminated=true;
    		finishExperiment();
    	}
    	 
    } else {
        hideElements();
        showConsent(); 
    }
   
}



/*
function validateAnalogy(){
  
    hideElements();  
    var mcq = $('#analogymcq').serializeArray();

    var ok = true;
    var empty=false;

    if (mcq.length === 0) {
		ok = false;
		empty=true;
     }else{
	     if(mcq[0].value != "correct") {
	        //alert('At least one answer was incorrect; please read the instructions and try again. You have one last attempt.');
	        ok = false;    
	    }

     }


    // goes to next section
    if (!ok) {
    	if (empty==true){
    		alert('Please answer this question to proceed.');
    		loadAnalogy();	
    	}
    	else{
    		alert('Incorrect answer. Experiment terminated.')
    		finishExperiment();
    	}
    	 
    } else {
        hideElements();
        showDemographics(); 
    }
    

}
*/

// show demographics form


function showConsent(){
	hideElements();
	$('#instructions').show();
    $('#instructions').load('html/consent.html');
    $('#next').show();
    $('#next').click(showDemographics);        

}



function showDemographics(){
	hideElements();
	$('#instructions').show();
    $('#instructions').load('html/demographics.html');
    $('#next').show();
    $('#next').click(validateDemographics);        

}


// ********** DEMOGRAPHICS: check to make sure demographic information is all okay
function validateDemographics() {
  
    demographics = $('#demo').serializeArray();
    var ok = true
    if (demographics.length != 2) {
      ok = false;
      alert('Please fill out all fields.');
      
    } else {

      for (var i = 0; i < demographics.length; i++) {
        // validate age
        if ((demographics[i].name == "age") && (/[^0-9]/.test(demographics[i].value))) {
            alert('Please only use numbers in age.');
            ok = false;
            break;
        }        
        // test for empty answers
        if (demographics[i].value === "") {
            alert('Please fill out all fields.');
            ok = false;
            break;
        }
   
      }
    }
	// goes to next section
    if (!ok) {
        
    } else {
    	exp_data['demographics']=demographics;   
        showGeneralInstructions();
    }
}


// ********** SHOWGENERALINSTRUCTIONS: displays general experiment instructions
function showGeneralInstructions() {

    hideElements();
    $('#instructions').show();
    $('#instructions').load('html/instructions.html');
    $('#next').show();
    $('#next').click(showInstructionChecks);
}


// ********** SHOWINSTRUCTIONCHECKS: asks the questions confirming they have read the instructions
function showInstructionChecks() {
    hideElements();
    $('#instructionchecks').show();
    $('#instructionchecks').load('html/instructionchecks.html');    
    $('#next').show();
    $('#next').click(validateInstructionChecks);
}


// ********** VALIDATEINSTRUCTIONCHECKS: makes sure they answered the questions about instructions correctly
function validateInstructionChecks() {
  
    hideElements();  
    instructionChecks = $('#instr').serializeArray();

    var ok = true;
    
    for (var i = 0; i < instructionChecks.length; i++) {
        // check for incorrect responses
        if(instructionChecks[i].value != "correct") {
            //alert('At least one answer was incorrect; please read the instructions and try again. You have one last attempt.');
            ok = false;
            break;
        }
        // check for empty answers 
        if (instructionChecks[i].value === "") {
            //alert('Please fill out all fields. You have one last attempt.');
            ok = false;
            break;    
        }
    }

    // where this is the number of questions in the instruction check
    if (instructionChecks.length != 3) {
        //alert('You have not answered all of the questions; please try again. You have one last attempt.');
        ok = false;
    }
	
    // goes to next section
    if (!ok) {
    	if (instructionAttempts==0){
    		instructionAttempts=instructionAttempts+1
    		alert('Incorrect or incomplete response. You have one more attempt')
        	showGeneralInstructions();	
    	}
    	else{
    		alert('Sorry. Experiment terminated.')
    		terminated=true;
    		finishExperiment();
    	}
    	 
    } else {
        hideElements();
        exp_data['instructionAttempts']=instructionAttempts;
        getReady(); 
    }
    	
}



// ********** GETREADY: shows the page right before beginning the SL sequence
function getReady() {
    hideElements();
    setExperiment();
    ind = 0; lastRepeat = 0;
    corrSLrepeats = 0; incorrSLrepeats = 0;  
    $('#instructions').show();
    $('#instructions').load('html/interim.html', function () {
      $('#message').text('Okay, you are all set!');
    });
    $('#next').show();
    $('#next').click(showStatisticalLearningTrial);
}


// ********** SETEXPERIMENT: sets all of the information for the experiment
function setExperiment() {
   
   	//change to automated loading and random initialization
   	
   	imageFolders=['lion', 'gorilla', 'flamingo', 'peacock', 'goldfish','ant', 'snail', 'iguana', 'pig', 'balloon', 'necklace', 'microwave', 'flute', 'taxi', 'candle', 'bucket', 'umbrella', 'desk'] 
   	
   	// imageFolders=['lion', 'gorilla', 'flamingo', 'peacock', 'taxi']
   	

   	shuffledFolders=shuffleArray(imageFolders)
   	// console.log(shuffledFolders)
   	
   	object1={
   		'lion': 'a lion',
   		'gorilla':'a gorilla',
   		'flamingo':'a flamingo',
   		'peacock':'a peacock',
   		'goldfish':'a goldfish',
   		'ant':'an ant',
   		'snail':'a snail',
   		'iguana':'an iguana',
   		'pig':  'a pig',
		'balloon'  :  'a balloon',
		'flute': 'a flute',
		'necklace'  : 'a necklace',
		'microwave': 'a microwave',
		'taxi' :  'a taxi',
		'candle'  :  'a candle', 
		'bucket'  : 'a bucket',
		'umbrella'  :  'an umbrella',
		'desk'  :   'a desk'
   	}


   	
   	imageFolderFeatureDict={
   		'lion':['can roar','has a mane', 'has a tail','has fur','has teeth', 'is ferocious', 'is large','lives in jungles', 'is yellow','lives in Africa'],
   		'gorilla':['likes to eats bananas','can swing from trees','is black','is dangerous','has fur/hair','is large','is strong','beats its chest'],
   		'flamingo':['can fly', 'can stand on one leg','has a beak','has a long neck','has feathers', 'has long legs', 'is pink', 'lives in water'],
   		'peacock':['has feathers', 'is colourful','has long tail feathers', 'is blue', 'is noisy/loud', 'is beautiful', 'has a beak','is proud'],
   		'goldfish':['can swim', 'has fins','has scales','is orange','is small', 'lives in aquariums','lives in water','lives in a bowl'],
   		'ant':['has legs','can bite','can crawl','is black','is red','is small','is strong','lives in colonies'],
   		'snail':['has a shell','is slow','is slimy','is found in gardens','has eyes','leaves a trail','has an antenna','is edible'],
   		'iguana':['is green', 'is scaly','has a tail','has a tongue','has legs', 'can eat insects','is big/large','lives in hot climates'],
   		'pig':  ['has a snout','is pink','has a curly tail','likes mud','is found on farms','is fat','oinks','can be eaten as bacon'],
		'balloon'  :  ['can float', 'made of rubber','is colourful','requires helium','can burst','is for parties','is round','requires air'],
		'flute':['made of metal','is long','has holes','used by blowing air through','is silver','produces music','used in orchestras','is thin'],
		'necklace'  : ['is worn around the neck','made of gold','made of pearls','made of silver','is expensive','has a clasp','has a pendant','is for females'],
		'microwave':['is found in kitchens','can cook food','can heat','made of metal','has a door/doors','is electric','is rectangular/square','is fast'],
		'taxi' :  ['is yellow', 'is black', 'made of metal','has a meter','used for transportation','is expensive','used for passengers','has a sign'],
		'candle'  :  ['has a wick','made of wax','provides light','produces heat','melts','is scented','different shapes','is romantic'], 
		'bucket'  :['has a handle/handles','made of metal','made of plastic','can contain liquid', 'used for holding things','is circular/round','is found on beaches','is used for gardening'],
		'umbrella'  :  ['protects from the rain','has a handle/handles','is watertight/waterproof','is collapsible','keeps things dry','made of fabric/cloth material','is carried','has spokes'],
		'desk'  :   ['made of wood','has legs','has drawers','is flat','found in offices','made of metal','is strong','used for working on']
   		
   	};

   	/*

   	
   	imageFolderFeatureDict={
   		'lion':['roars','has a mane', 'has a tail','has fur','has teeth', 'is ferocious', 'is large','lives in jungles', 'is yellow','lives in Africa'],
   		'gorilla':['eats bananas','swings from trees','is black','is dangerous','has fur/hair','is large','is strong','beats its chest'],
   		'flamingo':['flies', 'stands on one leg','has a beak','has a long neck','has feathers', 'has long legs', 'is pink', 'lives in water'],
   		'peacock':['has feathers', 'is colourful','has long tail feathers', 'is blue', 'is noisy/loud', 'is beautiful', 'has a beak','is proud'],
   		'goldfish':['swims', 'has fins','has scales','is orange','is small', 'lives in aquariums','lives in water','lives in a bowl'],
   		'ant':['has legs','bites','crawls','is black','is red','is small','is strong','lives in colonies'],
   		'snail':['has a shell','is slow','is slimy','is found in gardens','has eyes','leaves a trail','has an antenna','is edible'],
   		'iguana':['is green', 'is scaly','has a tail','has a tongue','has legs', 'eats insects','is big/large','lives in hot climates'],
   		'pig':  ['has a snout','is pink','has a curly tail','does like mud','is found on farms','is fat','does oink','eaten as bacon'],
 		// 'broccoli' : ['is green','is nutritious','has florets','is eaten with cheese sauce','is related to cabbages','has a stalk_stem','is eaten raw'],
		// 'cauliflower':['is white', 'is nutritious', 'does grow in ground','eaten raw','has florets','has green leaves','eaten by cooking','is eaten with cheese'],
		// 'corn':['is yellow', 'is on cobs','eaten as popcorn','has pips seeds','grows in fields', 'is edible','has kernels','tastes sweet'],
		// 'cucumber':['is green','is long','is eaten in salad','is made of water/is watery','has pips_seeds','has skin peel','is refreshing','is crunchy'],
		// 'pineapple'  :  ['is prickly','is yellow','has leaves','is sweet','has skin peel','is juicy','has flesh','grows on trees'],
		// 'strawberry':['is red','has pips seeds','is associated with summer','is sweet','is juicy','used for making jams','is small','has leaves'],
		'balloon'  :  ['floats', 'made of rubber','is colourful','requires helium','bursts','is for parties','is round','requires air'],
		'flute':['made of metal','is long','has holes','used by blowing air through','is silver','produces music','used in orchestras','is thin'],
		'necklace'  : ['is worn around the neck','made of gold','made of pearls','made of silver','is expensive','has a clasp','has a pendant','is for women females girls'],
		'microwave':['is found in kitchens','cooks food','does heat','made of metal','has a door/doors','is electric','is rectangular/square','is fast'],
		'taxi' :  ['is yellow', 'is black', 'made of metal','has a meter','used for transportation','is expensive','used for passengers','has a sign'],
		'candle'  :  ['has a wick','made of wax','provides light','produces heat','melts','is scented','different shapes','is romantic'], 
		'bucket'  :['has a handle/handles','made of metal','made of plastic','holds/contains liquid/water', 'used for holding things','is circular round','is found on beaches','is used for gardening'],
		'umbrella'  :  ['protects from the rain','has a handle/handles','is watertight/waterproof','is collapsible','keeps dry','made of fabric/cloth material','is carried','has spokes'],
		'desk'  :   ['made of wood','has legs','has drawers','is flat','found in offices','made of metal','is strong','used for working on']
   		
   	};

   	*/
   	
   	
   	for (var i = 0; i < imageFolders.length; i++) {
    	imageFolderFeatureDict[imageFolders[i]]=shuffleArray(imageFolderFeatureDict[imageFolders[i]].slice(0,8));
	}
   	
   	// console.log(imageFolderFeatureDict);
   	
  	
    for (var i = 0; i < imageFolders.length; i++) {
    	// randomnumber = Math.floor((Math.random() * 29) + 1);  	
    	randomnumber=Math.floor(Math.random() * (89 - 0 + 1)) + 0;
      randomImageNumbers.push(randomnumber);
    	imageCategoryNumberDict[shuffledFolders[i]]=randomnumber;
    }
  	console.log('changed')	
  	console.log(randomImageNumbers);
  	
  	exp_data['categoryOrder']=shuffledFolders;
  	exp_data['imageNumbers']=randomImageNumbers;
  	exp_data['imageAndCategoryMapping']=imageCategoryNumberDict;
  	exp_data['imageFolderFeatureDict']=imageFolderFeatureDict;
  	
}


// ********** SHOWSTATISTICALLEARNINGTRIAL: shows a single image in the standard SL task
function showStatisticalLearningTrial() {
  
  hideElements();
  
  pic = 'img2/' + shuffledFolders[ind]+'/'+randomImageNumbers[ind]+'.JPEG';
  $('#instructions').show();
  $('#instructions').load('html/showpicture.html', function () {
    $('#stimulus').attr('src',pic);
    $('#obj1').text(object1[shuffledFolders[ind]]);
    $('#obj2').text(shuffledFolders[ind]);
  });

  //change to feature instructions
  // console.log(randomnumber)
  //$('#instructionchecks').load('html/features.html');
  $('#instructionchecks').show();
  $('#instructionchecks').load('html/features.html', function (){
  	$('#q1').text(imageFolderFeatureDict[shuffledFolders[ind]][0]);
  	$('#q2').text(imageFolderFeatureDict[shuffledFolders[ind]][1]);
  	$('#q3').text(imageFolderFeatureDict[shuffledFolders[ind]][2]);
  	$('#q4').text(imageFolderFeatureDict[shuffledFolders[ind]][3]);
  	$('#q5').text(imageFolderFeatureDict[shuffledFolders[ind]][4]);
  	$('#q6').text(imageFolderFeatureDict[shuffledFolders[ind]][5]);
   	$('#q7').text(imageFolderFeatureDict[shuffledFolders[ind]][6]);
   	$('#q8').text(imageFolderFeatureDict[shuffledFolders[ind]][7]);
  
  });
  if(shuffledFolders[ind]=='lion'){
  	$('#instructionchecks2').show();
  	$('#instructionchecks2').load('html/extraFeatures.html', function (){
	  	$('#q9').text('is an animal');
  	});
  }


  if(shuffledFolders[ind]=='taxi'){
  	// console.log('t')
  	$('#instructionchecks2').show();
  	$('#instructionchecks2').load('html/extraFeatures.html', function (){
	  	$('#q9').text('is a vehicle');
  	});
  }

  
  $('#next').show();
  $('#next').click(validateFeatureResponse);
  //setTimeout(function() {removePicture()},longpauselen);  
}

//******************** VALIDATEFEATURERESPONSE
function validateFeatureResponse() {

	if (shuffledFolders[ind]!="lion" && shuffledFolders[ind]!="taxi"){
	    //hideElements();  
	    featuresCheck = $('#features').serializeArray();
	    var ok = true;

	    
	    // where this is the number of questions in the instruction check
	    if (featuresCheck.length != 8) {
	        alert('You have not answered all of the questions; please try again.');
	        ok = false;
	    }
	    
	    for (var i = 0; i < featuresCheck.length; i++) {
	        // check for empty answers 
	        if (featuresCheck[i].value === "") {
	            alert('Please fill out all fields.');
	            ok = false;
	            break;    
	        }
	        
	    }
	    
	    // goes to next section
	    if (!ok) {
	        //showStatisticalLearningTrial(); 
	    } else {
	    	responses.push(featuresCheck);
	    	responses_dict[shuffledFolders[ind]]=featuresCheck;
	        removePicture(); 
	    }

	}else{
		    //hideElements();  
	    featuresCheck = $('#features').serializeArray();
	    extraFeaturesCheck=$('#features2').serializeArray();
	    var ok = true;

	    
	    // where this is the number of questions in the instruction check
	    if (featuresCheck.length != 8 || extraFeaturesCheck.length!=1) {
	        alert('You have not answered all of the questions; please try again.');
	        ok = false;
	    }
	    
	    for (var i = 0; i < featuresCheck.length; i++) {
	        // check for empty answers 
	        if (featuresCheck[i].value === "") {
	            alert('Please fill out all fields.');
	            ok = false;
	            break;    
	        }
	        
	    }

	    if (extraFeaturesCheck.length === 0) {
	            alert('Please fill out all fields.');
	            ok = false;
		}

	    
	    // goes to next section
	    if (!ok) {
	        //showStatisticalLearningTrial(); 
	    } else {
	    	responses.push(featuresCheck);
	    	// console.log(extraFeaturesCheck)
	    	extraResponse[shuffledFolders[ind]]=extraFeaturesCheck;
	    	responses_dict[shuffledFolders[ind]]=featuresCheck;
	        removePicture(); 
	    }


	} 


}



// ********** REMOVEPICTURE: removes the picture for shortpauselen milliseconds
function removePicture() {
	hideElements();
	// console.log('hide');
  pic = 'img2/white.jpg';
  ind = ind+1;
  $('#instructions').show();
  $('#instructions').load('html/showpicture.html', function () {
    $('#stimulus').attr('src',pic);
  });
  if (ind < imageFolders.length) {
	if(debugging==1 && ind==3){
		finishExperiment();
	}
	else{
		$("#form").scrollTop();
		hideElements();
		setTimeout(function() {showStatisticalLearningTrial()},10); 
	}
  } else {
      hideElements();
      exp_data['responses']=responses;
      exp_data['responses_dict']=responses_dict;
      exp_data['extraResponse']=extraResponse;
      // console.log(exp_data)
      saveData(exp_data); 
      finishExperiment();
  }
}





// ********** FINISHEXPERIMENT: show final instructions
function finishExperiment() {
	if(terminated==false){
		hideElements();
    	showDebrief();	
	}else{
		hideElements();
	}
    
}



// ********** SHOWDEBRIEF: shows the debrief text for those interested
function showDebrief() {
    hideElements();
    $('#instructions').show();
    var str = 'html/debrief.html';
    $('#instructions').load(str, function () {
        $('#subid').text(subjectID.toString());
    });
}


// ********** SAVEDATA: writes that data to server
function saveData(args) {
    var data = args;
    (function (d) {
        $.post('submit', {"content": JSON.stringify(d)});
    })(data);
}


// ********** SHUFFLEARRAYNOREPEATS: permute the values of array with no repeated items
function shuffleArrayNoRepeats(array) {
    var currentIndex = array.length-1, temporaryValue, randomIndex;
    var repeat = true;
    while (-1 !== currentIndex) {
        repeat = true;
        while (repeat) {
          randomIndex = Math.floor(Math.random() * (array.length-1));
          while ( (randomIndex==currentIndex) || 
                randomIndex==(currentIndex+1) || randomIndex==(currentIndex-1)) {
                  randomIndex = Math.floor(Math.random() * (array.length-1));
                }
          repeat = isRepeat(array,array[currentIndex],randomIndex);
          if (repeat==false) {
            repeat = isRepeat(array,array[randomIndex],currentIndex);
          }
        }
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
        currentIndex = currentIndex - 1;
    }
    return array;
}


function shuffleArray(array) {
    var currentIndex = array.length, temporaryValue, randomIndex ;
    while (0 !== currentIndex) {
        currentIndex = currentIndex - 1;
        randomIndex = Math.floor(Math.random() * currentIndex);
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }
    return array;
}

// ********** CONTAINS: returns TRUE if item is in array, FALSE otherwise
function contains(array,item) {
    var doesContain = false;
    for (var i=0; i<array.length; i++) {
      if (array[i]==item) {
        doesContain = true;
        break;
      }   
    }
    return doesContain;
}


function removeItem(array,item) {
  var newArray = array;
  for (var i=0; i<array.length; i++) {
    if (newArray[i]==item) {
      newArray.splice(i,1);
      break;
    }
  }
  return newArray;
}

// ********** HIDEELEMENTS: hides all DOM elements from the screen and clears the canvas
function hideElements() {
  
  $('div').hide();  // hides all divs
  $(':button').hide(); // hides all buttons
  $(':button').unbind(); // unbinds all buttons
}
