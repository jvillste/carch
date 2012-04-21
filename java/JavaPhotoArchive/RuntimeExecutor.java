package JavaPhotoArchive;

import java.io.IOException;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeoutException;

public class RuntimeExecutor {

	public void execute(String command, long timeout) throws IOException, TimeoutException {
		Process p = Runtime.getRuntime().exec(command);

		// Set a timer to interrupt the process if it does not return within the
		// timeout period
		Timer timer = new Timer();
		timer.schedule(new InterruptScheduler(Thread.currentThread()), timeout);
		

		try {
			p.waitFor();
		} catch (InterruptedException e) {
			// Stop the process from running
			p.destroy();
			Thread.interrupted();	
			throw new TimeoutException(command + "did not return after " + timeout + " milliseconds");
		} finally {
			// Stop the timer
			timer.cancel();
		}


	}

	private class InterruptScheduler extends TimerTask {
		Thread target = null;

		public InterruptScheduler(Thread target) {
			this.target = target;
		}

		@Override
		public void run() {
			target.interrupt();
		}

	}

}
