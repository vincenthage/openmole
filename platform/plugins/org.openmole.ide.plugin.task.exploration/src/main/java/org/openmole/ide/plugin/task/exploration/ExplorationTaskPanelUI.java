/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * ExplorationTaskPanelUI.java
 *
 * Created on 21 avr. 2011, 11:18:10
 */
package org.openmole.ide.plugin.task.exploration;

import org.openmole.ide.core.properties.ITaskPanelUI;
import org.openmole.ide.core.properties.ITaskDataUI;
import javax.swing.JPanel;

/**
 *
 * @author mathieu
 */
public class ExplorationTaskPanelUI extends JPanel  implements ITaskPanelUI{
    /** Creates new form ExplorationTaskPanelUI */
    public ExplorationTaskPanelUI(ExplorationTaskDataUI pud) {
        initComponents();
    }

    public ITaskDataUI saveContent(String name) {return new ExplorationTaskDataUI(name);}
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 400, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 266, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables

}
