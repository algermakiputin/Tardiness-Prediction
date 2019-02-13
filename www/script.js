$(document).ready(function() {
	var height = $(document).height() - ( $(".navbar").height() + $(".topmenu ").height() + $("#class").height() + 200 );
	//$("#list").css("max-height", height + "px");
	$("#emp_list").on("click",".li",function(){

		$(".emp-list").removeClass("active");
		$(this).addClass("active");

		status = $(this).data("status");
		years = $(this).data("years");
		education = $(this).data("education");
		tenure = $(this).data("tenure");
		department = $(this).data("department");
		age = $(this).data("age");
		name = $(this).text();

		Shiny.setInputValue("coord:s", {
			status : status,
			years : years,
			education : education,
			tenure : tenure,
			department : department,
			age : age,
			name : name
		});

	});
	
	$("#search").on("keyup", function() {
    var value = $(this).val().toLowerCase();
    $("#emp_list li").filter(function() {
      $(this).toggle($(this).text().toLowerCase().indexOf(value) > -1)
    });
  });

	$("body").on("click", "#addnewemployee", function(){
		var name = $("[name=name]").val(); 
		var years = $("[name=years]").val();  
		var marital_status = $("[name=marital-status]").val();
		var age = $("[name=age]").val();
		var education = $("[name=education]").val();
		var tenure = $("[name=tenure]").val();
		var department = $("[name=department]").val();

		if (years == "") {
			alert("Years of experience is required");
			return;
		}

		if (age == "") {
			alert("Age is required");
			return;
		}

		if (tenure == "") {
			alert("Tenure is required");
			return;
		}

		$("#list").prepend("<li class="+ "li" +" data-status="+ ""+marital_status+"" 
			+" data-years="+ ""+years+"" 
			+" data-education="+ ""+education+"" 
			+" data-tenure="+ ""+tenure+"" 
			+" data-department="+ ""+department+"" +" data-age="+ ""+age+"" +">" + name  +"</li>")
		$("#shiny-modal").modal("toggle");
		alert("Employee Added Successfully");

	})
})