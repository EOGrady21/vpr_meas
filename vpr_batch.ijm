//@ File (label = "Input directory", style = "directory") input
//@ File (label = "Output directory", style = "directory") output
//@ String (label = "File suffix", value = ".tif") suffix
//@ string (label = 'Day') day
//@ string (label = 'Hour') hour


processFolder(input);

// function to scan folders/subfolders/files to find files with correct suffix
function processFolder(input) {
	list = getFileList(input);
	list = Array.sort(list);
	for (i = 0; i < list.length; i++) {
		if(File.isDirectory(input + File.separator + list[i]))
			processFolder(input + File.separator + list[i]);
		if(endsWith(list[i], suffix))
			processFile(input, output, list[i]);
	}
}

function processFile(input, output, file) {
	// set options
	setBatchMode(true);
	run("Set Measurements...", "area perimeter fit feret's display add redirect=None decimal=3");
	// open image file
	open(input + File.separator + file);
	id = getImageID();
	selectImage(id);
	// do processing
	run("RGB Color");
	run("8-bit");
	setAutoThreshold("Huang dark");
	//run("Threshold...");
	run("Analyze Particles...", "size=100-Infinty display exclude"); //exclude
}
// Save results
	filename = output + File.separator + "d"+day+".h"+hour+"meas_results.csv";
	run("Input/Output...", "jpeg=100 gif=-1 file=.csv save_column");
	saveAs("Results", filename);