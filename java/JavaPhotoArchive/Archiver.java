package JavaPhotoArchive;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.security.MessageDigest;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.ExecutionException;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.plugins.jpeg.JPEGImageWriteParam;
import javax.imageio.stream.FileImageInputStream;
import javax.imageio.stream.FileImageOutputStream;
import javax.imageio.stream.MemoryCacheImageInputStream;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingWorker;
import javax.swing.UIManager;
import javax.swing.GroupLayout.Alignment;

import com.drew.imaging.jpeg.JpegMetadataReader;
import com.drew.metadata.Directory;
import com.drew.metadata.Metadata;
import com.drew.metadata.exif.ExifDirectory;

public class Archiver extends JFrame{

	//JTextField sourcePath = new JTextField("C:\\Documents and Settings\\Jukka\\My Documents\\Lataukset\\kuva-arkisto\\2006-10-17");
	//JTextField targetPath = new JTextField("C:\\Documents and Settings\\Jukka\\My Documents\\Lataukset\\kuva-arkisto-pienet");
	JTextField sourcePath = new JTextField("");
	JTextField targetPath = new JTextField("");
	JCheckBox resize =  new JCheckBox();
	JLabel resizeLabel = new JLabel("Resize");
	JCheckBox preserveName =  new JCheckBox();
	JLabel preserveNameLabel = new JLabel("Preserve name");
	
	JTextArea log = new JTextArea(20,50);
	JButton archiveButton = new JButton("Archive");
	JButton cancelButton = new JButton("Stop");
	String logFileName = "";
	SwingWorker<Void,String> worker = null;
	
	public static void main(String[] args) throws Exception {
	/*
		File file = new File("C:\\Documents and Settings\\Jukka\\My Documents\\Lataukset\\kuva-arkisto\\2010-01-01\\2010-01-01.11.52.26_b96a5b996759cb39841cb368274960a5.jpg");
		resize(file,new File("C:\\Documents and Settings\\Jukka\\My Documents\\Lataukset\\kuva-arkisto-pienet\\target.jpg"));
	*/

		try {
			UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				new Archiver();
			}
		});

	}
	
	public Archiver(){
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		for(File root : File.listRoots())
		{
			String sourceDirectory = root.getAbsolutePath() + "DCIM";
			if(! root.getAbsolutePath().equals("A:\\"))
				if(new File(sourceDirectory).exists())
					sourcePath.setText(sourceDirectory);
		}
		
		if(new File(sourcePath.getText()).exists())
			writeLog(getFiles(new File(sourcePath.getText())).size() + " jpg files in the source directory.");
		
		String defaultTargetPath = "E:\\kuva-arkisto";

		if(new File(defaultTargetPath).exists())
			targetPath.setText(defaultTargetPath);
		
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		getContentPane().add(panel);
		
		JPanel configurationPanel = new JPanel();
		configurationPanel.setLayout(new BoxLayout(configurationPanel, BoxLayout.Y_AXIS));
		panel.add(configurationPanel, BorderLayout.PAGE_START);
		
		sourcePath.setAlignmentX(CENTER_ALIGNMENT);
		configurationPanel.add(sourcePath);
		targetPath.setAlignmentX(CENTER_ALIGNMENT);
		configurationPanel.add(targetPath);
		configurationPanel.add(resize);
		configurationPanel.add(resizeLabel);
		configurationPanel.add(preserveName);
		configurationPanel.add(preserveNameLabel);
		
		archiveButton.setAlignmentX(CENTER_ALIGNMENT);
		configurationPanel.add(archiveButton);
		cancelButton.setAlignmentX(CENTER_ALIGNMENT);
		configurationPanel.add(cancelButton);
		log.setLineWrap(false);
		log.setEditable(false);
		panel.add(new JScrollPane(log),BorderLayout.CENTER);

		pack();
		
		sourcePath.addFocusListener(new FocusListener(){

			@Override
			public void focusGained(FocusEvent arg0) {
				// TODO Auto-generated method stub
				
			}

			@Override
			public void focusLost(FocusEvent arg0) {
				if(new File(sourcePath.getText()).exists())
					writeLog(getFiles(new File(sourcePath.getText())).size() + " jpg files in the source directory.");
			}
			
		});
		
		archiveButton.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent e) {
				archive();
			}
		});
		
		cancelButton.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent e) {
				if(worker != null)
					worker.cancel(false);
			}
		});
		
		setVisible(true);
	}
	
	void writeLog(String logText)
	{
		log.setText(log.getText() + logText +"\n");
		log.revalidate();
	}
	
	void writeFileLog(String logText)
	{
		try {
			BufferedWriter writer = new BufferedWriter(new FileWriter(logFileName,true));
			writer.write(new SimpleDateFormat("yyyy-MM-dd HH:mm").format(new Date()) + " : " + logText) ;
			writer.newLine();
			writer.close() ;
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private void archive(){
		try {
			
			final File sourceDirectory = new File(sourcePath.getText());
			File targetDirectory = new File(targetPath.getText());
			
			if(!sourceDirectory.exists() || !targetDirectory.exists())
			{
				writeLog("Source or target directory does not exist.");
				return;
			}
			
			worker = new SwingWorker<Void, String>(){

				@Override
				protected Void doInBackground() throws Exception {
					
					ArrayList<File> files = getFiles(sourceDirectory);
					int i = 1;
					for(File file : files)
					{
						try{
							if(isCancelled())
								return null;
							
							String counter = "(" + i + "/" + files.size() + ") ";
							i++;
							
							String targetFilePath = getTargetFilePath(file);
							String targetFileName = file.getName();
							byte[] contents = null;
							
							if(!preserveName.isSelected())
							{
							    contents = getFileContents(file);
								targetFileName = getTargetFileName(contents,file);
							}

							File targetFile = new File(targetFilePath + "\\" + targetFileName); 

							if(targetFile.exists())
							{
								publish(counter + " Allready exists: " + targetFile.getAbsolutePath());
								continue;
							}
							
							new File(targetFilePath).mkdirs();
							
							if(resize.isSelected())
								resize(file,targetFile);
							else
								if(contents == null)
									copyFile(new ByteArrayInputStream(getFileContents(file)), targetFile);
								else
									copyFile(new ByteArrayInputStream(contents), targetFile);

							targetFile.setReadOnly();
							
							publish(counter + file.getAbsolutePath() + " --> " + targetFile.getAbsolutePath());
							
						}catch(Exception exception)
						{
						    StringWriter sw = new StringWriter();
						    PrintWriter pw = new PrintWriter(sw);
						    exception.printStackTrace(pw);
	
							publish("Error: " + exception.getMessage() + " " + sw.toString());
						}
					}
			
					return null;
				}


				
				@Override
				protected void process(List<String> logTexts) {
					for(String logText : logTexts)
					{
						writeFileLog(logText);
					}
				}
				
				@Override
				protected void done() {
					if(isCancelled())
						writeLog("Stopped.");
					else
						writeLog("Ready.");
				}

			};
			
			logFileName = targetPath.getText() + "\\archiver_log.txt";
			
			worker.execute();
			
			writeLog("Started. Logging to " + logFileName);
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private static byte[] getFileContents(File file) throws FileNotFoundException, IOException {
		FileInputStream fis  = new FileInputStream(file);
		byte[] contents = new byte[(int)file.length()];
		fis.read(contents);
		return contents;
	}
	

	private String getTargetFilePath(File file) throws Exception
	{

    	String targetFilePath = targetPath.getText() + "\\dateless";
    	
		Calendar photoDate = Calendar.getInstance();
		Date date = getPhotoDate(file);
		if(date != null)
		{
			photoDate.setTime(date);
		
			DecimalFormat format = new DecimalFormat("00");
			
			targetFilePath = targetPath.getText() + "\\" + photoDate.get(Calendar.YEAR)
			+ "\\" + photoDate.get(Calendar.YEAR)
			+ "-" + format.format(photoDate.get(Calendar.MONTH) + 1)
			+ "-" + format.format(photoDate.get(Calendar.DAY_OF_MONTH));
			
		}
		return targetFilePath;
	}
	
	private String getTargetFileName(byte[] contents, File file) throws Exception
	{
		String md5 = getMD5(new ByteArrayInputStream(contents));
		String targetFileName = md5 + ".jpg";
		
		Calendar photoDate = Calendar.getInstance();
		Date date = getPhotoDate(file);
		if(date != null)
		{
			photoDate.setTime(date);
		
			DecimalFormat format = new DecimalFormat("00");

			targetFileName = photoDate.get(Calendar.YEAR)
			+ "-" + format.format((photoDate.get(Calendar.MONTH) + 1))
			+ "-" + format.format(photoDate.get(Calendar.DAY_OF_MONTH))
			+ "." + format.format(photoDate.get(Calendar.HOUR_OF_DAY))
			+ "." + format.format(photoDate.get(Calendar.MINUTE))
			+ "." + format.format(photoDate.get(Calendar.SECOND))
			+ "_" + md5
			+ ".jpg";
		}
		
		return targetFileName;
	}
	
	private static Date getPhotoDate(File file) throws Exception
	{
		Metadata metadata = JpegMetadataReader.readMetadata(file);

		Directory exifDirectory = metadata.getDirectory(ExifDirectory.class);
		if(exifDirectory.containsTag(ExifDirectory.TAG_DATETIME_DIGITIZED))
			return exifDirectory.getDate(ExifDirectory.TAG_DATETIME_DIGITIZED);
		else
			return null;
	}
	
	public static String getMD5(ByteArrayInputStream inputStream)
	{
		try {

			MessageDigest digest = java.security.MessageDigest.getInstance("MD5");
			 
		    byte[] dataBytes = new byte[1024];
		    int nread = inputStream.read(dataBytes);
		    while (nread > 0) {
		      digest.update(dataBytes, 0, nread);
		      nread = inputStream.read(dataBytes);
		    };
	
		    inputStream.close();

		    return getHexString(digest.digest());
		} catch (Exception e) {

			e.printStackTrace();
		}
		
	    return "";
	}
	
	
	public static String getHexString(byte[] b) throws Exception {
	  String result = "";
	  for (int i=0; i < b.length; i++) {
	    result +=
	          Integer.toString( ( b[i] & 0xff ) + 0x100, 16).substring( 1 );
	  }
	  return result;
	}

	private static ArrayList<File> getFiles(File directory)
	{
		ArrayList<File> list = new ArrayList<File>();
		getFiles(directory, list);
		return list;
	}
	
	private static void getFiles(File directory, ArrayList<File> list)
	{
		if(directory != null)
			for(File file : directory.listFiles())
			{
				if(file.isDirectory())
					getFiles(file, list);
				else
					if(file.getName().toLowerCase().endsWith(".jpg"))
						list.add(file);
			}
	}
	
	public static void resize(File inputFile, File targetFile) throws Exception
	{
		String command = ".\\convert.exe \"" + inputFile.getAbsolutePath() + "\" -resize 1024x1024 -quality 90 \"" + targetFile.getAbsolutePath()+ "\"";
		new RuntimeExecutor().execute(command, 20000);
	}
	
	 public static void copyFile(ByteArrayInputStream is, File out) throws Exception {
		    
		    FileOutputStream fos = new FileOutputStream(out);
		    try {
		        byte[] buf = new byte[1024];
		        int i = 0;
		        while ((i = is.read(buf)) != -1) {
		            fos.write(buf, 0, i);
		        }
		    } 
		    catch (Exception e) {
		        throw e;
		    }
		    finally {
		        if (is != null) is.close();
		        if (fos != null) fos.close();
		    }
	 }

}
